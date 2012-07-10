%% dl_cdb_adapter.erl
%% @doc This is the dripline couchdb adapter.  It is responsible
%%      for relaying messages from couchdb to dripline, and also
%%      for pushing updates from dripline back to couchdb.  It listens
%%      on both the couchdb changes feed *and* on the dripline softbus.
%%      When data is presented on the softbus that it believes should
%%      be logged to couchdb, it will automagically push it upstream.
%%      When configuration changes come down the couchdb changes feed,
%%      it will push them onto the softbus.  Commands are handled by
%%      spawning a process which will handle the command and subsequently
%%      push the result back to couch.
%%
%%      Couchbeam is intentionally 'firewalled' in this module to avoid
%%      couchbeam dependencies being splattered all over dripline.  
%%      The hope is that this will, in the future, provide for a more
%%      generic adapter structure.
-module(dl_cdb_adapter).
-behavior(gen_dl_agent).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% State records and such %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(cdb_loc, {
	  host, 
	  port
	 }).
-record(state, {
	  revs,
	  db_cnf_hndl,
	  db_cmd_hndl,
	  conf_ch_ref,
	  cmd_ch_ref,
	  db_loc = #cdb_loc{}
	 }).

%%%%%%%%%%%
%%% API %%%
%%%%%%%%%%%
%-export([notify/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_dl_agent callbacks %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([start_link/2,
	 init/1,
	 handle_sb_msg/2,
	 handle_info/2,
	 handle_cast/2,
	 handle_call/3,
	 code_change/3,
	 terminate/2]).

%%%%%%%%%%%%%%%%%%%%%
%%% Callback defs %%%
%%%%%%%%%%%%%%%%%%%%%
start_link(?MODULE, _Args) ->
    gen_dl_agent:start_link(?MODULE, ?MODULE).

init(_Args) ->
    {ok, {Host,Port}} = application:get_env(dl_core, couch_host),
    DbConn = couchbeam:server_connection(Host,Port),
    {ok, ConfDbHndl} = couchbeam:open_db(DbConn, "dripline_conf"),
    {ok, CmdDbHndl} = couchbeam:open_db(DbConn, "dripline_cmd"),
    {ok, ConfRef} = setup_conf_streaming(ConfDbHndl),
    {ok, CmdRef} = setup_cmd_streaming(CmdDbHndl),
    InitialState = #state{
      revs = dict:new(),
      db_cnf_hndl = ConfDbHndl,
      db_cmd_hndl = CmdDbHndl,
      conf_ch_ref = ConfRef,
      cmd_ch_ref = CmdRef,
      db_loc = #cdb_loc{
	host = Host,
	port = Port
       }
     },
    {ok, InitialState}.

%% Because this process can push messages into the softbus, we ignore
%% messages that we actually sent.
handle_sb_msg({_Ref, dl_cdb_adapter, _Msg}, #state{}=State) ->
    {noreply, State};
%% All other softbus messages are handled here.
handle_sb_msg({_Ref, _AnyID, {nd, {Instr, Chan}, Data}}, #state{}=SD) ->
    spawn(fun() -> worker_dt(Instr,Chan,Data) end),
    {noreply, SD};
handle_sb_msg({_Ref, _AnyID, _Msg}, #state{}=State) ->
    {noreply, State}.

%% When our streams go down, recuisitate them
handle_info({change, R, {done, _LastSeq}}, 
	    #state{cmd_ch_ref=R,db_cmd_hndl=H}=State) ->
    {ok, CmdRef} = setup_cmd_streaming(H),
    {noreply, State#state{cmd_ch_ref=CmdRef}};
handle_info({change, R, {done, _LastSeq}}, 
	    #state{conf_ch_ref=R,db_cnf_hndl=H}=State) ->
    {ok, ConfRef} = setup_conf_streaming(H),
    {noreply, State#state{conf_ch_ref=ConfRef}};
%% We get two kinds of changes.  The first kind comes from the 
%% configuration stream:
handle_info({change, R, ChangeData}, #state{conf_ch_ref=R, revs=Revs}=State) ->
    dl_softbus:bcast(agents, ?MODULE, ChangeData),
    {noreply, NewState};
%% The second kind of changes come from the command stream.
handle_info({change, R, ChangeData}, #state{cmd_ch_ref=R, revs=Revs, db_cmd_hndl=H}=State) ->
    NewState = case ignore_update_rev(ChangeData, Revs) of
		   true ->
		       State;
		   false ->
		       {ok, BFA} = dl_compiler:compile(ChangeData),
		       MFA = case BFA of
				 {{prologix, _, _}, F, A} ->
				     {gen_prologix, F, A};
				 {{unix, _, _}, read, A} ->
				     {gen_os_cmd, execute, A};
				 {system, get, heartbeat} ->
				     BFA;
				 Other ->
				     Other
			     end,
		       case node_is_endpoint(BFA) of
			   true ->
			       ID = props:get('doc._id',ChangeData),
			       spawn(fun() -> 
					     worker(MFA,ID,H) 
				     end);
			   false ->
			       lager:debug("recvd cmd for another node.")
		       end,
		       State#state{revs=update_rev_data(ChangeData,Revs)}
	       end,
    {noreply, NewState}.

handle_call(_Call, _From, StateData) ->
    {reply, ok, StateData}.

handle_cast(_Cast, StateData) ->
    {noreply, StateData}.

code_change(_Version, StateData, _Extra) ->
    {ok, StateData}.

terminate(_Reason, _StateData) ->
    ok.

%%%%%%%%%%%%%%%%
%%% Internal %%%
%%%%%%%%%%%%%%%%

%%----------------------------------------------------------------------%%
%% @doc setup_conf_streaming sets us up so that configuration changes are
%%      sent over the changes API to us.  Whereas with the command stream
%%      we want all of the changes since the last seq, with configuration
%%      we actually want all of them ever.  This gives us, on startup, the
%%      'latest' configuration documents that are there - which 
%%      effectively sets up the system.
%%----------------------------------------------------------------------%%
-spec setup_conf_streaming(couchbeam:db()) -> ok | {error, term()}.
setup_conf_streaming(DbHandle) ->
    StreamOpts = [continuous, include_docs, {since, 0}],
    case couchbeam_changes:stream(DbHandle, self(), StreamOpts) of
	{ok, StartRef, _ChPid} ->
	    {ok, StartRef};
	{error, _Error}=E ->
	    E
    end.

%%----------------------------------------------------------------------%%
%% @doc setup_cmd_streaming sets up the adapter to receive commands via
%%      couchdb.  This is a little different from the configuration feed
%%      because we definitely do not want to process every command since
%%      the dawn of the database.  Instead, we only get changes to the DB
%%      since the last sequence number.
%%----------------------------------------------------------------------%%
-spec setup_cmd_streaming(couchbeam:db()) -> ok | {error, term()}.
setup_cmd_streaming(DbHandle) ->
    {ok,Info} = couchbeam:db_info(DbHandle),
    LastSeq = props:get('update_seq',Info),
    StreamOpts = [continuous, include_docs, {since, LastSeq}],
    case couchbeam_changes:stream(DbHandle, self(), StreamOpts) of
	{ok, StartRef, _ChPid} ->
	    {ok, StartRef};
	{error, _Error}=E ->
	    E
    end.

%%---------------------------------------------------------------------%%
%% @doc update_rev_data adds an ignore flag to the revision dictionary 
%%		for a given document and revision tag.
%% @end
%%---------------------------------------------------------------------%%
update_rev_data(ChangeData,RevisionInfo) ->
	Id = props:get('doc._id',ChangeData),
	BinRev = props:get('doc._rev',ChangeData),
	Rev = strip_rev_no(BinRev),
	dict:store(Id,Rev + 1,RevisionInfo).

%%----------------------------------------------------------------------%%
%% @doc When a document passes through the changes feed, we automatically
%%      ignore the next update.  This should work just fine.
%%----------------------------------------------------------------------%%
-spec ignore_update_rev(ejson:json_object(), dict()) -> boolean().
ignore_update_rev(ChangeLine, RevsDict) ->
    Type = props:get('doc.type', ChangeLine),
    DocID = props:get('doc._id', ChangeLine),
    BinRev = props:get('doc._rev', ChangeLine),
    RevNo = strip_rev_no(BinRev),
    case dict:find(DocID, RevsDict) of
	{ok, RevNo} ->
	    true;
	_ ->
	    false
    end.

%%---------------------------------------------------------------------%%
%% @doc strip_rev_no/1 takes a binary "_rev" tag and strips the revision
%% 		sequence number.  this is very useful for notifying the monitor
%%		that a sequence number is about to be changed by e.g. us.
%% @end
%%---------------------------------------------------------------------%%
-spec strip_rev_no(binary()) -> integer().
strip_rev_no(BinRev) ->
    [NS,_] = string:tokens(binary_to_list(BinRev),"-"),
    {N,[]} = string:to_integer(NS),
    N.

%%----------------------------------------------------------------------%%
%% @doc The worker for this module is responsible for going off and 
%%      actually fetching a result without blocking the main adapter.  
%%      When it gets a response, it will update couch with the appropriate
%%      answer.  
%%----------------------------------------------------------------------%%
-spec worker(term(),binary(),couchbeam:db()) -> ok.
worker({system, get, heartbeat}, DocID, DbHandle) ->
    Result = [{<<"result">>,<<"thump">>},{<<"final">>,<<"thump">>}],
    update_couch_doc(DbHandle, DocID, Result);
worker({dl_sys=M, F, A}, DocID, DbHandle) ->
    Res = dl_data_to_couch(do_dl_sys(M,F,A)),
    update_couch_doc(DbHandle,DocID,Res);
worker({M, F, [InstrName,ChLoc|_Rest]=A}, DocID, DbHandle) ->
    Result = case M of
		 gen_prologix ->
		     Res = erlang:apply(M,F,A),
		     DlDt = dl_data:from_prologix(Res),
		     ChInfo = dl_conf_mgr:channel_info(InstrName, ChLoc),
		     ChName = dl_ch_data:get_id(ChInfo),
		     HookedData = try
				      dl_hooks:apply_hooks(ChName,DlDt)
				  catch
				      C:E ->
					  lager:info("failed to apply hooks for channel ~p (~p:~p)",
						     [ChName,C,E]),
					  DlDt
				  end,
		     dl_data_to_couch(HookedData);
		 gen_os_cmd ->
		     Res = erlang:apply(M,F,A),
		     DlDt = dl_data:new(),
		     DlDt2 = dl_data:set_code(DlDt, ok),
		     DlDt3 = dl_data:set_data(DlDt2, Res),
		     ChInfo = dl_conf_mgr:channel_info(InstrName, ChLoc),
		     HookedData = try
				      ChName = dl_ch_data:get_id(ChInfo),
				      dl_hooks:apply_hooks(ChName,DlDt3)
				  catch
				      _C:_E ->
					  DlDt3
				  end,
		     dl_data_to_couch(HookedData)
	     end,
    lager:debug("worker will update doc ~p with result ~p",[DocID,Result]),
    update_couch_doc(DbHandle, DocID, Result).

%%----------------------------------------------------------------------%%
%% @doc This is a specialized worker for responding to data takers.  It
%%      takes data *already read* and pushes it up to the data log on 
%%      the couch database.
%%----------------------------------------------------------------------%%
-spec worker_dt(atom(),atom(),binary()) -> ok.
worker_dt(Instr,Ch,RawData) ->
    DlDt = dl_data:from_prologix(RawData),
    ChInfo = dl_conf_mgr:channel_info(Instr, Ch),
    ChName = dl_ch_data:get_id(ChInfo),
    HookedData = try
		     dl_hooks:apply_hooks(ChName,DlDt)
		 catch
		     C:E ->
			 lager:info("failed to apply hooks for channel ~p (~p:~p) [~p,~p]",
				    [ChName,C,E,DlDt,ChInfo]),
			 DlDt
		 end,
    CouchDoc = dl_dt_data_to_couch(ChName,HookedData),
    post_dt_couch_doc(CouchDoc).

%%----------------------------------------------------------------------%%
%% @doc Post a data point to couchdb by creating a new document.
%%----------------------------------------------------------------------%%
-spec post_dt_couch_doc(term()) -> ok.
post_dt_couch_doc(CD) ->
    NewDoc = couchbeam_doc:extend(CD,{[]}),
    {ok, {Host,Port}} = application:get_env(dl_core, couch_host),
    DbConn = couchbeam:server_connection(Host,Port),
    {ok, Db} = couchbeam:open_or_create_db(DbConn,"dripline_logged_data"),
    {ok, _Doc} = couchbeam:save_doc(Db,NewDoc).

%%----------------------------------------------------------------------%%
%% @doc Update couch doc with a result.  Pretty straightforward, uses
%%      couchbeam.
%%----------------------------------------------------------------------%%
-spec update_couch_doc(couchbeam:db(),binary(),term()) -> ok | {error, term()}.
update_couch_doc(DbHandle, DocID, Props) ->
    {ok, Doc} = couchbeam:open_doc(DbHandle, DocID),
    NewDoc = couchbeam_doc:extend(Props, Doc),
    case couchbeam:save_doc(DbHandle, NewDoc) of
	{ok, _} ->
	    ok;
	{error, conflict} ->
	    ok;
	{error, _Other}=Err ->
	    Err
    end.

%%----------------------------------------------------------------------%%
%% @doc Interrogate the configuration manager to determine if the node
%%      dripline is running on can respond to a given compiled result.
%%----------------------------------------------------------------------%%
-spec node_is_endpoint(term()) -> boolean().
node_is_endpoint({{unix, BusID, _}, _F, _A}) ->
    lager:debug("bus ~p interrogated.",[BusID]),
    LocalIDs = [dl_bus_data:get_id(X) || X <- dl_conf_mgr:local_buses()],
    lists:member(BusID, LocalIDs);
node_is_endpoint({{prologix, BusID, _}, _F, _A}) ->
    lager:debug("bus ~p interrogated.",[BusID]),
    LocalIDs = [dl_bus_data:get_id(X) || X <- dl_conf_mgr:local_buses()],
    lists:member(BusID, LocalIDs);
node_is_endpoint({system, get, heartbeat}) ->
    true;
node_is_endpoint({dl_sys, start_loggers, Args}) ->
    lists:foldl(fun(_Ch, false) ->
			false;
		   (Ch, true) ->
			dl_conf_mgr:is_local_channel(Ch)
		end,
		true, 
		Args);
node_is_endpoint({dl_sys, stop_loggers, Args}) ->
    lists:foldl(fun(_Ch, false) ->
			false;
		   (Ch, true) ->
			dl_conf_mgr:is_local_channel(Ch)
		end,
		true, 
		Args);
node_is_endpoint({dl_sys, current_loggers, []}) ->
    true.

%%----------------------------------------------------------------------%%
%% @doc do_dl_sys goes out to the system, performs a task, and then 
%%      coerces the result into something appropriate for couch.
%%----------------------------------------------------------------------%%
-spec do_dl_sys(atom(),atom(),[atom()]) -> ejson:json_object().
do_dl_sys(dl_sys=M,start_loggers=F,A) ->
    Res = erlang:apply(M,F,[A]),
    Dt = dl_data:new(),
    dl_sys_loggers_to_couch(Res,Dt);
do_dl_sys(dl_sys=M,stop_loggers=F,A) ->
    Res = erlang:apply(M,F,[A]),
    Dt = dl_data:new(),
    dl_sys_loggers_to_couch(Res,Dt);
do_dl_sys(dl_sys=M,current_loggers=F,[]) ->
    Res = erlang:apply(M,F,[]),
    Dt = dl_data:new(),
    LgList = lists:map(fun(X) ->
			       erlang:atom_to_binary(X,latin1)
		       end,
		       Res),
    dl_sys_logger_list_to_couch(LgList, Dt).

-spec dl_sys_logger_list_to_couch([binary()],dl_data:dl_data()) ->
					 ejson:json_object().
dl_sys_logger_list_to_couch(LgList, Data) ->
    Dt = dl_data:set_data(Data,LgList),
    Dt2 = dl_data:set_code(Dt,ok),
    dl_data:set_ts(Dt2,dl_util:make_ts()).

-spec dl_sys_loggers_to_couch([{atom(), atom() | {atom(), atom()}}],
			      dl_data:dl_data()) ->
				     ejson:json_object().
dl_sys_loggers_to_couch(Result, Data) ->
    {Code, ResProps} = dl_sys_loggers_result_to_props(Result),
    Dt = dl_data:set_data(Data, {ResProps}),
    Dt2 = dl_data:set_code(Dt, Code),
    dl_data:set_ts(Dt2, dl_util:make_ts()).

dl_sys_loggers_result_to_props(Res) ->
    dl_sys_loggers_result_to_props(Res, []).
dl_sys_loggers_result_to_props([],Acc) ->
    {ok, Acc};
dl_sys_loggers_result_to_props([{_A,B}=Tup|T],Acc) when is_atom(B) ->
    dl_sys_loggers_result_to_props(T,[encode_tuple(Tup)|Acc]);
dl_sys_loggers_result_to_props([{A,{error, {already_started, _}}}|T],Acc) ->
    Tup = {erlang:atom_to_binary(A,latin1),{[encode_tuple({error,already_started})]}},
    dl_sys_loggers_result_to_props(T,[Tup|Acc]);
dl_sys_loggers_result_to_props([{A,{error, no_logger}}|T],Acc) ->
    Tup = {erlang:atom_to_binary(A,latin1),{[encode_tuple({error,no_logger_conf})]}},
    dl_sys_loggers_result_to_props(T,[Tup|Acc]);
dl_sys_loggers_result_to_props([H|T],Acc) when is_atom(H) ->
    dl_sys_loggers_result_to_props(T,[erlang:atom_to_binary(H,latin1)|Acc]).

encode_tuple({A,B})->
    {erlang:atom_to_binary(A,latin1), erlang:atom_to_binary(B,latin1)}.


%%----------------------------------------------------------------------%%
%% @doc dl_data_to_couch just translates a dl_data structure into a 
%%      couch-friendly representation.  In this case, it is a proplist of
%%      binary pairs.
%%----------------------------------------------------------------------%%
-spec dl_data_to_couch(dl_data:dl_data()) -> [{binary(),binary()}].
dl_data_to_couch(DlDt) ->
    case dl_data:get_code(DlDt) of
	error ->
	    Ts = dl_data:get_ts(DlDt),
	    Fn = dl_data:get_final(DlDt),
	    [
	     {<<"result">>, <<"error">>},
	     {<<"timestamp">>, Ts},
	     {<<"final">>, {Fn}}
	    ];
	ok ->
	    Rs = dl_data:get_data(DlDt),
	    Ts = dl_data:get_ts(DlDt),
	    Fn = dl_data:get_final(DlDt),
	    [
	     {<<"result">>, Rs},
	     {<<"timestamp">>, Ts},
	     {<<"final">>, Fn}
	    ]
    end.

%%----------------------------------------------------------------------%%
%% @doc dl_dt_data_to_couch just translates a dl_data structure into a 
%%      couch-friendly representation.  In this case, it is a proplist of
%%      binary pairs and has been collected by a data taker.
%%----------------------------------------------------------------------%%
-spec dl_dt_data_to_couch(atom(),dl_data:dl_data()) -> [{binary(),binary()}].
dl_dt_data_to_couch(Name,DlDt) ->
    SName = erlang:atom_to_binary(Name,latin1),
    case dl_data:get_code(DlDt) of
	ok ->
	    Rs = dl_data:get_data(DlDt),
	    Ts = dl_data:get_ts(DlDt),
	    Fn = dl_data:get_final(DlDt),
	    [
	     {<<"sensor_name">>, SName},
	     {<<"uncalibrated_value">>, Rs},
	     {<<"timestamp_localstring">>, Ts},
	     {<<"calibrated_value">>, Fn}
	    ]
    end.
	    

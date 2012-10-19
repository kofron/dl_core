%% dl_conf_mgr.erl
%% @doc The dripline configuration manager module.  Uses
%%      Mnesia as a backend for storing configuration data.
-module(dl_conf_mgr).
-behavior(gen_dl_agent).

-record(state,{id}).

-export([start_link/2,
	 init/1,
	 handle_sb_msg/2,
	 handle_info/2,
	 handle_cast/2,
	 handle_call/3,
	 code_change/3,
	 terminate/2]).

%%%%%%%%%%%%%%%%%%%%%%%%
%%% Includes for QLC %%%
%%%%%%%%%%%%%%%%%%%%%%%%
-include_lib("stdlib/include/qlc.hrl").

%%%%%%%%%%%%%%%%%%%%%
%%% API Functions %%%
%%%%%%%%%%%%%%%%%%%%%
-export([channel_info/1,channel_info/2,is_local_channel/1]).
-export([instrument_info/1]).
-export([local_buses/0,bus_info/1]).
-export([logger_info/1,local_loggers/0,running_loggers/0]).

-export([get_read_mfa/1]).
-export([get_write_mfa/1]).

%%%%%%%%%%%%%%%%%%%%%%%
%%% API Definitions %%%
%%%%%%%%%%%%%%%%%%%%%%%
local_loggers() ->
    gen_dl_agent:call(?MODULE, local_lgs).

running_loggers() ->
    gen_dl_agent:call(?MODULE, run_lgs).

local_buses() ->
    gen_dl_agent:call(?MODULE, local_bs).

channel_info(Ch) ->
    gen_dl_agent:call(?MODULE, {info, ch, Ch}).

channel_info(In,Loc) ->
    gen_dl_agent:call(?MODULE, {info, ch, {In, Loc}}).

logger_info(Channel) ->
    gen_dl_agent:call(?MODULE, {info, lg, Channel}).

instrument_info(In) ->
    gen_dl_agent:call(?MODULE, {info, in, In}).

bus_info(Bs) ->
    gen_dl_agent:call(?MODULE, {info, bs, Bs}).

get_read_mfa(ChannelName) ->
    gen_dl_agent:call(?MODULE, {mfa, read, ch, ChannelName}).

get_write_mfa(ChannelName) ->
    gen_dl_agent:call(?MODULE, {mfa, write, ch, ChannelName}).

start_link(?MODULE, _Args) ->
    gen_dl_agent:start_link(?MODULE, ?MODULE).

init([ID|_T]) ->
    ok = create_mnesia_tables(),
    {ok, #state{id=ID}}.

%% Handle messages coming from the couchdb adapter.  These are going
%% to be mostly configuration changes, as that's what the adapter likes
%% to pump into the softbus.  Also, ignore messages that we sent into
%% the softbus.
handle_sb_msg({_Ref, ?MODULE, _Msg}, #state{}=State) ->
    {noreply, State};
handle_sb_msg({_Ref, dl_cdb_adapter, Msg}, #state{}=State) ->
    maybe_update_tables(Msg),
    {noreply, State};
handle_sb_msg({_Ref, Pid, {dtb, _LgId, ChData, _Ival}}, State) ->		    
    ChName = dl_ch_data:get_id(ChData),
    record_logger_pid(ChName, Pid),
    {noreply, State};
handle_sb_msg({_Ref, _AnyID, _Msg}, #state{}=State) ->
    {noreply, State}.

handle_info({'DOWN', _MRef, process, Obj, _Info}, #state{}=SD) when is_pid(Obj) ->
    io:format("logger went down!~p~n",[Obj]),
    LgInfo = get_lg_by_pid(Obj),
    record_logger_pid(dl_dt_data:get_channel(LgInfo),undefined),
    {noreply, SD}.

handle_call(local_lgs, _From, StateData) ->
    {reply, local_lgs(), StateData};
handle_call(run_lgs, _From, StateData) ->
    {reply, run_lgs(), StateData};
handle_call(local_bs, _From, StateData) ->
    {reply, get_local_bss(), StateData};
handle_call({info, ch, {In, Loc}}, _From, StateData) ->
    Reply = case get_rev_ch_data(In, Loc) of
		{ok, Data} ->
		    Data;
		{error, _Reason}=E ->
		    E
	    end,
    {reply, Reply, StateData};
handle_call({info, ch, Ch}, _From, StateData) ->
    Reply = case get_ch_data(Ch) of
		{ok, Data} ->
		    Data;
		{error, _Reason}=E ->
		    E
	    end,
    {reply, Reply, StateData};
handle_call({info, in, In}, _From, StateData) ->
    Reply = case get_instr_data(In) of
		{ok, Data} ->
		    Data;
		{error, _Reason}=E ->
		    E
	    end,
    {reply, Reply, StateData};
handle_call({info, bs, Bs}, _From, StateData) ->
    Reply = case get_bus_data(Bs) of
		{ok, Data} ->
		    Data;
		{error, _Reason}=E ->
		    E
	    end,
    {reply, Reply, StateData};
handle_call({info, lg, Ch}, _From, StateData) ->
    Reply = case get_dt_data(Ch) of
		{ok, Data} ->
		    Data;
		{error, _Reason}=E ->
		    E
	    end,
    {reply, Reply, StateData};
handle_call({mfa, read, ch, ChName}, _From, StateData) ->
    Reply = case get_ch_mfa(ChName, read) of
		{ok, MFA} ->
		    MFA;
		{error, _Reason}=E ->
		    E
	    end,
    {reply, Reply, StateData};
handle_call({mfa, write, ch, ChName}, _From, StateData) ->
    Reply = case get_ch_mfa(ChName, write) of
		{ok, MFA} ->
		    MFA;
		{error, _Reason}=E ->
		    E
	    end,
    {reply, Reply, StateData}.


handle_cast(_Cast, StateData) ->
    {noreply, StateData}.

code_change(_Version, StateData, _Extra) ->
    {ok, StateData}.

terminate(_Reason, _StateData) ->
    ok.

%%%%%%%%%%%%%%%%
%%% Internal %%%
%%%%%%%%%%%%%%%%
-spec create_mnesia_tables() -> ok | term().
create_mnesia_tables() ->
    ok = create_ch_data_table(),
    ok = create_instr_data_table(),
    ok = create_bus_data_table(),
    ok = create_dt_data_table().

-spec create_ch_data_table() -> ok | term().
create_ch_data_table() ->
    case mnesia:create_table(dl_ch_data,
			     [
			      {ram_copies, [node()]},
			      {attributes, dl_ch_data:fields()}
			     ]) of
	{atomic, ok} ->
	    ok;
	{aborted,{already_exists,dl_ch_data}} ->
	    ok;
	AnyOther ->
	    AnyOther
    end.

-spec create_instr_data_table() -> ok | term().
create_instr_data_table() ->
    case mnesia:create_table(dl_instr_data,
			     [
			      {ram_copies, [node()]},
			      {attributes, dl_instr_data:fields()}
			     ]) of
	{atomic, ok} ->
	    ok;
	{aborted,{already_exists,dl_instr_data}} ->
	    ok;
	AnyOther ->
	    AnyOther
    end.

-spec create_bus_data_table() -> ok | term().
create_bus_data_table() ->
    case mnesia:create_table(dl_bus_data,
			     [
			      {ram_copies, [node()]},
			      {attributes, dl_bus_data:fields()}
			     ]) of
	{atomic, ok} ->
	    ok;
	{aborted,{already_exists,dl_bus_data}} ->
	    ok;
	AnyOther ->
	    AnyOther
    end.

-spec create_dt_data_table() -> ok | term().
create_dt_data_table() ->
    case mnesia:create_table(dl_dt_data,
			     [
			      {ram_copies, [node()]},
			      {attributes, dl_dt_data:fields()}
			     ]) of
	{atomic, ok} ->
	    ok;
	{aborted,{already_exists,dl_dt_data}} ->
	    ok;
	AnyOther ->
	    AnyOther
    end.

-spec local_lgs() -> [atom()].
local_lgs() ->
    Qs = qlc:q([dl_dt_data:get_channel(Lg) 
		|| Lg <- mnesia:table(dl_dt_data),
		   is_local_channel(dl_dt_data:get_channel(Lg))]),
    {atomic, Ans} = mnesia:transaction(fun() ->
					       qlc:e(Qs)
				       end),
    Ans.

-spec get_lg_by_pid(pid()) -> [atom()].
get_lg_by_pid(Pid) ->
    Qs = qlc:q([Lg
		|| Lg <- mnesia:table(dl_dt_data),
		   dl_dt_data:get_pid(Lg) == Pid]),
    {atomic, Ans} = mnesia:transaction(fun() ->
					       qlc:e(Qs)
				       end),
    case Ans of
	[] ->
	    [];
	[Lgr] ->
	    Lgr
    end. 

-spec run_lgs() -> [atom()].
run_lgs() ->
    Qs = qlc:q([dl_dt_data:get_channel(Lg) 
		|| Lg <- mnesia:table(dl_dt_data),
		   dl_dt_data:get_pid(Lg) =/= undefined]),
    {atomic, Ans} = mnesia:transaction(fun() ->
					       qlc:e(Qs)
				       end),
    Ans.

-spec get_local_bss() -> [atom()].
get_local_bss() ->
    NodeName = dl_util:node_name(),
    Qs = qlc:q([Bs || Bs <- mnesia:table(dl_bus_data),
		      dl_bus_data:get_node(Bs) == NodeName
	       ]),
    {atomic, Ans} = mnesia:transaction(fun() ->
					       qlc:e(Qs)
				       end),
    Ans.

-spec instr_on_bus(atom()) -> [dl_ch_data:ch_data()].
instr_on_bus(BusName) ->
    Mid = fun({_,X,_}) -> X end,
    Qs = qlc:q([In || In <- mnesia:table(dl_instr_data),
		      Mid(dl_instr_data:get_bus(In)) == BusName]),
    {atomic, Ans} = mnesia:transaction(fun() ->
					       qlc:e(Qs)
				       end),
    Ans.

-spec get_ch_data(atom()) -> {ok, dl_ch_data:ch_data()}
				 | {error, term()}.
get_ch_data(ChName) ->
    Qs = qlc:q([Ch || Ch <- mnesia:table(dl_ch_data),
		      dl_ch_data:get_id(Ch) == ChName
	       ]),
    {atomic, Ans} = mnesia:transaction(fun() ->
					       qlc:e(Qs)
				       end),
    case Ans of
	[] ->
	    {error, no_channel};
	[H] ->
	    {ok, H}
    end.

-spec get_dt_data(atom()) -> {ok, dl_dt_data:dt_data()}
				 | {error, term()}.
get_dt_data(ChName) ->
    Qs = qlc:q([Dt || Dt <- mnesia:table(dl_dt_data),
		      dl_dt_data:get_channel(Dt) == ChName
	       ]),
    {atomic, Ans} = mnesia:transaction(fun() ->
					       qlc:e(Qs)
				       end),
    case Ans of
	[] ->
	    {error, no_logger};
	[H] ->
	    {ok, H}
    end.

-spec get_rev_ch_data(atom(), atom()) -> {ok, dl_ch_data:ch_data()}
				 | {error, term()}.
get_rev_ch_data(Instrument, Locator) ->
    Qs = qlc:q([Ch || Ch <- mnesia:table(dl_ch_data),
		      dl_ch_data:get_locator(Ch) == Locator,
		      dl_ch_data:get_instr(Ch) == Instrument]),
    {atomic, Ans} = mnesia:transaction(fun() ->
					       qlc:e(Qs)
				       end),
    case Ans of
	[] ->
	    {error, no_ch};
	[H] ->
	    {ok, H}
    end.				       

-spec get_bus_data(atom()) -> {ok, dl_bus_data:bus_data()}
				 | {error, term()}.
get_bus_data(BsName) ->
    Qs = qlc:q([Ch || Ch <- mnesia:table(dl_bus_data),
		      dl_bus_data:get_id(Ch) == BsName
	       ]),
    {atomic, Ans} = mnesia:transaction(fun() ->
					       qlc:e(Qs)
				       end),
    case Ans of
	[] ->
	    {error, no_bus};
	[H] ->
	    {ok, H}
    end.

-spec get_ch_mfa(atom(), atom()) -> {ok, term()} | {error, term()}.
get_ch_mfa(ChannelName, Action) ->
    Qc = qlc:q([
		Ch || Ch <- mnesia:table(dl_ch_data)
	       ]),
    Qi = qlc:q([
		In || In <- mnesia:table(dl_instr_data)
	       ]),
    Qs = qlc:q([{dl_instr_data:get_bus(I),
		 Action,
		 [dl_instr_data:get_id(I),dl_ch_data:get_locator(C)]} ||
		   C <- Qc, 
		   I <- Qi,
		   dl_instr_data:get_id(I) =:= dl_ch_data:get_instr(C),
		   dl_ch_data:get_id(C) =:= ChannelName
	       ]),
    {atomic, Ans} = mnesia:transaction(fun() ->
					       qlc:e(Qs)
				       end),
    case Ans of
	[] ->
	    {error, no_channel};
	[H] ->
	    {ok, H}
    end.

-spec get_instr_data(atom()) -> {ok, dl_instr_data:dl_instr_data()}
				 | {error, term()}.
get_instr_data(InName) ->
    Qs = qlc:q([In || In <- mnesia:table(dl_instr_data),
		      dl_instr_data:get_id(In) == InName
	       ]),
    {atomic, Ans} = mnesia:transaction(fun() ->
					       qlc:e(Qs)
				       end),
    case Ans of
	[] ->
	    {error, no_instrument};
	[H] ->
	    {ok, H}
    end.

-spec maybe_update_tables(ejson:json_object()) -> ok.
maybe_update_tables(Msg) ->
    case props:get('doc.type',Msg) of
	<<"channel">> ->
	    update_channel_table(Msg);
	<<"instrument">> ->
	    update_instrument_table(Msg);
	<<"bus">> ->
	    update_bus_table(Msg);
	<<"logger">> ->
	    update_logger_table(Msg);
	Other ->
	    declare_nonsense(Msg)
    end.
	
-spec update_channel_table(ejson:json_object()) -> ok.
update_channel_table(Msg) ->	      
    Doc = props:get('doc', Msg),
    {ok, ChData} = dl_ch_data:from_json(Doc),
    maybe_add_or_update_channel(ChData).

-spec update_instrument_table(ejson:json_object()) -> ok.
update_instrument_table(Msg) ->
    Doc = props:get('doc',Msg),
    {ok, InData} = dl_instr_data:from_json(Doc),
    maybe_add_or_update_instrument(InData).

-spec update_bus_table(ejson:json_object()) -> ok.
update_bus_table(Msg) ->
    Doc = props:get('doc',Msg),
    {ok, BsData} = dl_bus_data:from_json(Doc),
    maybe_add_or_update_bus(BsData).

-spec update_logger_table(ejson:json_object()) -> ok.
update_logger_table(Msg) ->
    Doc = props:get('doc',Msg),
    {ok, DtData} = dl_dt_data:from_json(Doc),
    maybe_add_or_update_logger(DtData).

-spec declare_nonsense(ejson:json_object()) -> ok.
declare_nonsense(Msg) ->
    lager:debug("unhandled msg recvd by conf mgr: ~p",[Msg]).

-spec maybe_add_or_update_channel(dl_ch_data:ch_data()) -> ok.
maybe_add_or_update_channel(ChData) ->
    case get_ch_data(dl_ch_data:get_id(ChData)) of
	{error, no_channel} ->
	    lager:info("new channel recvd: ~p",[ChData]),
	    add_channel(ChData);
	{ok, ChData} ->
	    lager:debug("ignoring redundant channel conf (~p = ~p)",[ChData,ChData]);
	{ok, OldChData} ->
	    lager:debug("overwriting conf for channel: (~p != ~p)",[ChData, OldChData]),
	    update_channel(ChData)
    end.

-spec maybe_add_or_update_logger(dl_dt_data:ch_data()) -> ok.
maybe_add_or_update_logger(LgData) ->
    case get_dt_data(dl_dt_data:get_channel(LgData)) of
	{error, no_logger} ->
	    lager:info("new logger recvd: ~p",[LgData]),
	    add_logger(LgData);
	{ok, LgData} ->
	    lager:debug("ignoring redundant logger conf");
	{ok, NewLgData} ->
	    lager:debug("overwriting conf for logger: ~p",[NewLgData]),
	    update_logger(LgData)
    end.

-spec maybe_add_or_update_bus(dl_bus_data:bus_data()) -> ok.
maybe_add_or_update_bus(ChData) ->
    case get_bus_data(dl_bus_data:get_id(ChData)) of
	{error, no_bus} ->
	    lager:info("new bus recvd: ~p",[ChData]),
	    add_bus(ChData);
	{ok, ChData} ->
	    lager:debug("ignoring redundant bus conf");
	{ok, NewChData} ->
	    lager:debug("overwriting conf for bus: ~p",[NewChData]),
	    update_bus(ChData)
    end.

-spec maybe_add_or_update_instrument(dl_instr_data:dl_instr_data()) -> ok.
maybe_add_or_update_instrument(InData) ->
    case get_instr_data(dl_instr_data:get_id(InData)) of
	{error, no_instrument} ->
	    lager:info("new instrument recvd: ~p",[InData]),
	    add_instrument(InData);
	{ok, InData} ->
	    lager:debug("ignoring redundant instrument conf");
	{ok, NewInData} ->
	    lager:debug("overwriting conf for instrument: ~p",[NewInData]),
	    update_instrument(InData)
    end.
		
-spec add_instrument(dl_instr_data:dl_instr_data()) -> ok.
add_instrument(InData) ->    
    F = fun() ->
		mnesia:write(InData)
	end,
    {atomic, ok} = mnesia:transaction(F),
    {_BusMod, BusName, _BusAddr} = dl_instr_data:get_bus(InData),
    IsLocalInstr = lists:member(BusName, 
				lists:map(fun dl_bus_data:get_id/1,get_local_bss())),
    case IsLocalInstr of
	true ->
	    lager:info("starting local instrument (~p)",[InData]),
	    ok = try_instr_start(InData);
	false ->
	    ok
    end,
    dl_softbus:bcast(agents, ?MODULE, {nin, InData}).

-spec update_instrument(dl_instr_data:dl_instr_data()) -> ok.
update_instrument(InData) ->
    F = fun() ->
		mnesia:write(InData)
	end,
    {atomic, ok} = mnesia:transaction(F),
    dl_softbus:bcast(agents, ?MODULE, {uin, dl_instr_data:get_id(InData)}).

-spec add_channel(dl_ch_data:ch_data()) -> ok.
add_channel(ChData) ->
    F = fun() ->
		mnesia:write(ChData)
	end,
    {atomic, ok} = mnesia:transaction(F),
    dl_softbus:bcast(agents, ?MODULE, {nch, ChData}).

-spec add_logger(dl_dt_data:dt_data()) -> ok.
add_logger(DtData) ->
    F = fun() ->
		mnesia:write(DtData)
	end,
    {atomic, ok} = mnesia:transaction(F),
    dl_softbus:bcast(agents, ?MODULE, {ndt, DtData}).

-spec update_logger(dl_dt_data:dt_data()) -> ok.
update_logger(DtData) ->
    F = fun() ->
		mnesia:write(DtData)
	end,
    {atomic, ok} = mnesia:transaction(F),
    dl_softbus:bcast(agents, ?MODULE, {udt, DtData}).

-spec is_local_channel(atom()) -> boolean().
is_local_channel(ChName) ->
    {ok, Dt} = get_ch_data(ChName),
    is_local_instr(dl_ch_data:get_instr(Dt)).

-spec is_local_instr(atom()) -> boolean().
is_local_instr(InstrName) ->
    {ok, Dt} = get_instr_data(InstrName),
    {_, BusName, _} = dl_instr_data:get_bus(Dt),
    lists:member(BusName, lists:map(fun(X) -> 
					    dl_bus_data:get_id(X) 
				    end,
				    get_local_bss())).

-spec record_logger_pid(atom(),pid()) -> ok.
record_logger_pid(ChName,LgPid) ->
    io:format("recording ~p~n",[LgPid]),
    Qs = qlc:q([Lg || Lg <- mnesia:table(dl_dt_data),
		      dl_dt_data:get_channel(Lg) == ChName]),
    {atomic, ok} = mnesia:transaction(fun() ->
					       [Dt] = qlc:e(Qs),
					       Dtp = dl_dt_data:set_pid(Dt,LgPid),
					       mnesia:write(Dtp)
				       end),
    start_logger_monitor(LgPid).
		
-spec start_logger_monitor(pid() | undefined) -> ok.
start_logger_monitor(undefined) ->
    ok;
start_logger_monitor(LgPid) ->
    erlang:monitor(process, LgPid).
   
-spec update_channel(dl_ch_data:ch_data()) -> ok.
update_channel(ChData) ->
    F = fun() ->
		mnesia:write(ChData)
	end,
    {atomic, ok} = mnesia:transaction(F),
    dl_softbus:bcast(agents, ?MODULE, {uch, dl_ch_data:get_id(ChData)}).

-spec add_bus(dl_bus_data:bus_data()) -> ok.
add_bus(BsData) ->
    F = fun() ->
		mnesia:write(BsData)
	end,
    {atomic, ok} = mnesia:transaction(F),
    BusName = dl_bus_data:get_id(BsData),
    IsLocalBus = lists:member(BsData, get_local_bss()),
    case IsLocalBus of
	true ->
	    ok = try_bus_start(BsData),
	    Instr = instr_on_bus(BusName),
	    lists:foreach(fun(I) ->
				  ok = try_instr_start(I)
			  end,
			  Instr);
	false ->
	    lager:info("non-local bus info recvd (~p)",[BusName]),
	    ok
    end,
    dl_softbus:bcast(agents, ?MODULE, {nbs, BsData}).

-spec update_bus(dl_bus_data:bus_data()) -> ok.
update_bus(BsData) ->
    F = fun() ->
		mnesia:write(BsData)
	end,
    {atomic, ok} = mnesia:transaction(F),
    dl_softbus:bcast(agents, ?MODULE, {ubs, dl_bus_data:get_id(BsData)}).

-spec try_bus_start(dl_bus_data:dl_bus_data()) -> ok.
try_bus_start(BsData) ->
    try
	dl_instr:start_bus(BsData)
    catch
	Err:Reason ->
	    lager:warning("couldn't start bus!!! ~p:~p",[Err,Reason])
    end.

-spec try_instr_start(dl_instr_data:dl_instr_data()) -> ok.
try_instr_start(InData) ->
    try 
	dl_instr:start_instr(InData)
    catch
	C:E ->
	    lager:warning("Couldn't call dl_instr:start_instr! (~p~p)",[C,E])
    end,
    ok.

-module(dl_compiler).
-behaviour(gen_server).

%%%%%%%%%%%
%%% API %%%
%%%%%%%%%%%
-export([compile/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server api and callbacks %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% internal server state %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(state, {}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% intermediate data record %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(intermed, {type, do, channel, value}).

%%%%%%%%%%%%%%%%%%%%%%%
%%% API definitions %%%
%%%%%%%%%%%%%%%%%%%%%%%
-spec compile(ejson:json_object()) -> 
		     {ok, fun()} | {error, dl_error:error()}.
compile(JSON) ->
    gen_server:call(?MODULE,{compile, JSON}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server API and callback definitions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok, #state{}}.

handle_call({compile, JS}, _From, State) ->
    Reply = case drip_compile(JS) of
		{ok, _F}=Success ->
		    Success;
		{error, _E}=Err ->
		    Err
	    end,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% internal functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec drip_compile(ejson:json_object()) -> 
			  {ok, fun()} | {error, dl_error:error()}.
drip_compile(JS) ->
    New = #intermed{},
    case compile_to_rec(JS,New) of
	{ok, Rec} ->
	    case compile_to_mfa(Rec) of
		{ok, _F}=Success -> 
		    Success;
		Error -> 
		    Error
	    end;
	Err ->
	    Err
    end.

-spec compile_to_rec(ejson:json_object(),#intermed{}) ->
			    {ok, #intermed{}} | {error, dl_error:error()}.
compile_to_rec(JS,I) ->
    resolve_type(JS,I).

-spec resolve_type(ejson:ejson_object(), #intermed{}) ->
			  {ok, binary()} | {error, notype}.
resolve_type(JS, Inter) ->
    case props:get(doc.type,JS,undefined) of
	undefined ->
	    dl_error:compiler_expected(type_field,no_type_field);
	Type ->
	    case lists:member(Type, type_tokens()) of
		true ->
		    AType = dl_util:binary_to_atom(Type),
		    resolve_action(JS,Inter#intermed{type=AType});
		false ->
		    dl_error:compiler_surprised(<<"type">>,Type)
	    end
    end.

-spec type_tokens() -> [binary()].
type_tokens() ->
    [
     <<"command">>,
     <<"system">>,
     <<"channel">>,
     <<"instrument">>
    ].

-spec resolve_action(ejson:ejson_object(), #intermed{}) ->
			    {ok, #intermed{}} | dl_error:error().
resolve_action(JS,#intermed{type=command}=I) ->
    case props:get(doc.command,JS) of
	undefined ->
	    dl_error:field_undefined(compiler,command);
	_Cmd ->
	    case props:get(doc.command.do,JS) of
		undefined ->
		    dl_error:field_undefined(compiler,do);
		Do ->
		    case lists:member(Do,action_tokens()) of
			true ->
			    ADo = dl_util:binary_to_atom(Do),
			    resolve_target(JS,I#intermed{do=ADo});
			false ->
			    dl_error:compiler_surprised(do,Do)
		    end
	    end
    end.

-spec action_tokens() -> [binary()].
action_tokens() ->
    [
     <<"get">>,
     <<"set">>,
     <<"run">>
    ].

-spec resolve_target(ejson:json_object(),#intermed{}) -> 
			    {ok, #intermed{}} | dl_error:error().
resolve_target(JS,#intermed{type=command,do=run}=I) ->
    Cmd = props:get('doc.command',JS),
    Tgt = case props:get('channel',Cmd) of
	      undefined ->
		  mantis;
	      Ch ->
		  erlang:binary_to_atom(Ch, latin1)
	  end,
    Cmd2 = props:take(['rate','duration','output'],Cmd),
    Data = props:to_proplist(Cmd2),
    {ok, I#intermed{channel=Tgt,value=Data}};
resolve_target(JS,#intermed{type=command,do=get}=I) ->
    case props:get(doc.command.channel,JS) of
	undefined ->
	    dl_error:field_undefined(compiler,channel);
	Ch ->
	    {ok, I#intermed{channel=erlang:binary_to_atom(Ch,latin1)}}
    end;
resolve_target(JS,#intermed{type=command,do=set}=I) ->	
    case props:get(doc.command.channel,JS) of
	undefined ->
	    dl_error:field_undefined(compiler,channel);
	Ch ->
	    case props:get(doc.command.value,JS) of
		undefined ->
		    dl_error:field_undefined(compiler,value);
		Val ->
		    Tgt = erlang:binary_to_atom(Ch,latin1),
		    {ok, I#intermed{channel=Tgt,value=Val}}
	    end
    end.

-spec compile_to_mfa(#intermed{}) -> {ok, term()}.
compile_to_mfa(#intermed{type=command, do=run, value=V}) ->
    Args = gen_run_params(V),
    {ok, {{unix, ignatius, 0}, read, [mantis, Args]}};
compile_to_mfa(#intermed{type=command, do=get, channel=heartbeat}) ->
    {ok, {system, get, heartbeat}};
compile_to_mfa(#intermed{type=command, do=get, channel=Ch}) ->
    {ok, dl_conf_mgr:get_read_mfa(Ch)};
compile_to_mfa(#intermed{type=command, do=set, channel=Ch, value=Val}) ->
    {{A, B, C}, D, Args} = dl_conf_mgr:get_write_mfa(Ch),
    {ok, {{A,B,C}, D, Args ++ [Val]}}.

-spec gen_run_params(ejson:json_object()) -> [{atom(), string()}].
gen_run_params(P) ->
    lists:map(fun({K,V}) ->
		      {erlang:binary_to_atom(K,latin1), 
		       erlang:binary_to_list(V)}
	      end,
	      P).

%%%%%%%%%%%%%
%%% EUNIT %%%
%%%%%%%%%%%%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.

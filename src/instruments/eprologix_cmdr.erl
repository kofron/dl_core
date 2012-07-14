-module(eprologix_cmdr).

-behaviour(gen_server).

%% API
-export([send/3,send_sync/3]).
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% internal server state %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(state,{id,ip,port,sock,c,q,term,waiting}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% internal request record %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(req,{addr,qs,res,sndr,faf}).

%%%%%%%%%%%
%%% API %%%
%%%%%%%%%%%
send_sync(ServerID, GPIBAddr, QueryString) ->
	Req = #req{
		addr = GPIBAddr,
		qs = QueryString,
		res = none,
		sndr = none,
		faf = false
	},
	gen_server:call(ServerID,Req).
send(ServerID, GPIBAddr, QueryString) ->
	Req = #req{
		addr = GPIBAddr,
		qs = QueryString,
		res = none,
		sndr = none,
		faf = true
	},
	gen_server:call(ServerID,Req).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server API and callback definitions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link(IPAddr, Port, FSMId) ->
	gen_server:start_link({local,FSMId},?MODULE,[IPAddr,Port,FSMId],[]).

init([IPAddr,Port,FSMId]) ->
	case get_telnet_sock(IPAddr,Port) of
		{ok, S} ->
			InitialState = #state{
			  id = FSMId,
			  port = Port,
			  ip = IPAddr,
			  q = [],
			  term = "\r\n",
			  sock = S,
			  waiting = false
			},
			{ok, InitialState};
		{error, _}=E ->
			E
	end.

handle_call(#req{faf=false}=R, F, #state{sock=S,waiting=false}=SD) ->
    NewStateData = SD#state{
		     c = R#req{sndr=F},
		     waiting = true
		  },
    CmdString = cmd_string(R,SD),
    ok = gen_tcp:send(S, CmdString),
    erlang:send_after(2000,self(),tcp_timeout), % We need to neglect 'lost' packets due to instrument failure.
    {noreply, NewStateData};
handle_call(#req{faf=true}=R, _F, #state{sock=S}=SD) ->
    CmdString = cmd_string(R,SD),
    ok = gen_tcp:send(S,CmdString),
    {reply, ok, SD};
handle_call(#req{}=R, F, #state{q=Q,waiting=true}=SD) ->
    NewStateData = SD#state{
		     q = Q ++ [R#req{sndr=F}]
		    },
    {noreply, NewStateData}.

handle_cast(_Msg, State) ->
  {noreply, State}.
handle_info(tcp_timeout, #state{c=#req{sndr=F},sock=S,q=[]}=SD) ->
    gen_server:reply(F,{error, tcp_timeout}),
    {noreply, SD#state{waiting=false}};
handle_info({tcp,S,D}, #state{c=#req{sndr=F},sock=S,q=[]}=SD) ->
    gen_server:reply(F,D),
    {noreply, SD#state{waiting=false}};
handle_info({tcp,S,_D}=P, #state{c=undefined,sock=S,q=[]}=SD) ->
    error_logger:warning("got unexpected packet out-of-turn: ~p~n",[P]),
    {noreply, SD};
handle_info({tcp,S,D}, #state{c=#req{sndr=F},sock=S,q=[N|T]}=SD) ->
    gen_server:reply(F,D),
    CmdStr = cmd_string(N, SD),
    ok = gen_tcp:send(S, CmdStr),
    NewSD = SD#state{
	      c = N,
	      q = T
     },
    {noreply, NewSD}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% internal functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
cmd_string(#req{addr=A,qs=Q,faf=RB}, #state{term=T}) ->
    NStr = lists:flatten(io_lib:format("~B",[A])),
    case RB of
	false ->
	    ["++addr", NStr, T, Q, T, "++read eoi", T];
	true ->
	    ["++addr", NStr, T, Q, T]
    end.
	    

get_telnet_sock(IPAddr,Port) ->
	case gen_tcp:connect(IPAddr,Port,[binary,{packet,0}]) of
		{ok, _}=Success ->	
			Success;
		{error, _}=Failure ->
			Failure
	end.


% gen_prologix.erl
% Behavior for instruments connected to Prologix TCP/IP to GPIB
% adapters.
-module(gen_prologix).

-behaviour(gen_server).

-export([behaviour_info/1]).

-export([init/1,start_link/4]).
-export([handle_call/3,handle_cast/2,terminate/2,code_change/3,handle_info/2]).

-export([read/2,write/3]).

-record(ep_st,{ep_id, gpib_addr}).
-record(pro_st,{mod, mod_sd, ep_d}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Behavior callback requirements %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
behaviour_info(callbacks) ->
    [
     {init,1},
     {start_link,3},
     {do_read,2},
     {do_write,3}
    ];
behaviour_info(_) ->
    undefined.

%%%%%%%%%%%
%%% API %%%
%%%%%%%%%%%
read(Instrument, Channel) ->
    gen_server:call(Instrument, {r, Instrument, Channel}).

write(Instrument, Channel, NewValue) ->
    gen_server:call(Instrument, {w, Instrument, Channel, NewValue}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server API and callback definitions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link(CallbackMod, ID, EProID, GPIBAddr) ->
    Args = [CallbackMod, ID, EProID, GPIBAddr],
    gen_server:start_link({local, ID}, ?MODULE, Args, []).

init([CallbackMod,_ID,EProID,GPIBAddr]=Args) ->
    case CallbackMod:init(Args) of
	{ok, ModStateData} = _StartOK ->
	    StateData = #pro_st{
		  mod = CallbackMod,
	      mod_sd = ModStateData,
	      ep_d = #ep_st{
		ep_id = EProID,
		gpib_addr = GPIBAddr
	       }
	     },
	    {ok, StateData};
	{ok, ToSend, ModStateData} = _StartOKWithInit ->
	    StateData = #pro_st{
		  mod = CallbackMod,
	      mod_sd = ModStateData,
	      ep_d = #ep_st{
		ep_id = EProID,
		gpib_addr = GPIBAddr
	       }
	     },
	    eprologix_cmdr:send(EProID, 
				GPIBAddr,
				ToSend),
	    {ok, StateData};
	StartFailed ->
	    StartFailed
    end.

handle_call({r, _In, Ch}, _From, #pro_st{mod=M,mod_sd=MS,ep_d=E}=St) ->
    {Rp,NMSDt} = case M:do_read(Ch,MS) of
		     {data, D, NewSD} ->
			 {D, NewSD};
		     {send, ToSend, NewSD} ->
			 R = eprologix_cmdr:send_sync(E#ep_st.ep_id,
						     E#ep_st.gpib_addr,
						     ToSend),
			 {{R, dl_util:make_ts()}, NewSD};
		     {send_then_parse, ToSend, NewSD} ->
			 R = eprologix_cmdr:send_sync(E#ep_st.ep_id,
						      E#ep_st.gpib_addr,
						      ToSend),
			 {PR, NewNewSD} = case M:do_parse(R, NewSD) of
					      {ok, Parsed, StateData} ->
						  {Parsed, StateData};
					      {error, Reason, StateData} ->
						  {{error, Reason}, StateData}
					  end,
			 {{PR, dl_util:make_ts()}, NewNewSD};
		     {error, Reason, NewSD} ->
			{{error, Reason}, NewSD};
		     {update_cache, NewSD} ->
			 {ok, ToSend, NewSDP} = M:do_update_cache(NewSD),
			 ActList = generate_action_list(ToSend),
			 R = evaluate_action_list(ActList, E#ep_st.ep_id,
						  E#ep_st.gpib_addr),
			 {ok, NewNewSD} = M:parse_instrument_reply(R, NewSDP),
			 case M:do_read(Ch, NewNewSD) of
			     {data, D, SDPPP} ->
				 {D, SDPPP};
			     _Other ->
				 {{error, max_cmd_depth_exceeded}, NewNewSD}
			 end;
		    {stop, _NewSD}=Die ->
			Die
		end,
    NewState = St#pro_st{mod_sd = NMSDt},
    {reply, Rp, NewState};
handle_call({w, _In, Ch, V}, _F, #pro_st{mod=M,mod_sd=MS,ep_d=E}=St) ->
    {Rp, NMSDt} = case M:do_write(Ch,V,MS) of
		    {data, D, NewSD} ->
			{D, NewSD};
		    {send, ToSend, NewSD} ->
			R = eprologix_cmdr:send(E#ep_st.ep_id,
						E#ep_st.gpib_addr,
						ToSend),
			 {{R, dl_util:make_ts()}, NewSD};
		    {error, Reason, NewSD} ->
			{{error, Reason}, NewSD};
		    {stop, _NewSD}=Die ->
			Die
		  end,
    NewState = St#pro_st{mod_sd = NMSDt},
    {reply, Rp, NewState}.

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
-spec generate_action_list([term()]) -> [fun()].
generate_action_list([]) ->
    [];
generate_action_list(Stuff) ->
    generate_action_list(Stuff, []).
generate_action_list([], Acc) ->
    lists:reverse(Acc);
generate_action_list([{sleep, NMilliSecs}|T],Acc) ->
    Append = fun(_,_) ->
		     timer:sleep(NMilliSecs)
	     end,
    generate_action_list(T, [Append|Acc]);
generate_action_list(RawList, Acc) ->
    case lists:takewhile(fun not_a_tuple/1, RawList) of
	List when List == RawList ->
	    Append = fun(Id,Addr) ->
			     eprologix_cmdr:send_sync(Id, Addr, RawList)
		     end,
	    generate_action_list([], [Append|Acc]);
	List ->
	    Append = fun(Id,Addr) ->
			     eprologix_cmdr:send(Id, Addr, List)
		     end,
	    MatchLen = erlang:length(List),
	    generate_action_list(lists:nthtail(MatchLen,RawList),[Append|Acc])
    end.

-spec not_a_tuple(term()) -> boolean().
not_a_tuple(Term) ->
    not erlang:is_tuple(Term).

evaluate_action_list([H|[]], Id, Addr) ->
    H(Id, Addr);
evaluate_action_list([H|T],Id,Addr) ->
    H(Id, Addr),
    evaluate_action_list(T,Id,Addr).

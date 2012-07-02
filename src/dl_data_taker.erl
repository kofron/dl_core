% dl_data_taker.erl
% @author Jared Kofron <jared.kofron@gmail.com>
% @doc The data taker is responsible for periodically
%      polling a channel and placing the data on the 
%      softbus.  Anybody who wants it can deal with it.
-module(dl_data_taker).
-behaviour(gen_dl_agent).

%%%%%%%%%%%
%%% API %%%
%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server api and callbacks %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, handle_sb_msg/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% internal server state %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(state, {id, tgt, tref, ival}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server API and callback definitions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link(TgtChanId, Interval) ->
    ID = gen_dt_id(TgtChanId),
    Args = [ID, TgtChanId, Interval],
    gen_dl_agent:start_link(?MODULE,ID,Args).

init([_ID, _CMod, ID, TgtChan, Interval]=Args) ->
    case dl_conf_mgr:channel_info(TgtChan) of
	{error, no_channel} ->
	    problematic_startup(Args);
	ChannelData ->
	    normal_startup([ChannelData,ID,TgtChan,Interval])
    end.

problematic_startup(_Args) ->
    {error, nostart}.

normal_startup([ChData, ID, _TgtChan, Interval]=_Args) ->
    TRef = start_countdown(Interval),
    InitialState = #state{
      id = ID,
      tgt = ChData,
      ival = Interval,
      tref = TRef
     },
    {ok, InitialState}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(do_record, #state{ival=I,tgt=T}=SD) ->
    Branch = case do_collect_data(T) of
		 ok ->
		     NewTRef = start_countdown(I),
		     {noreply, SD#state{tref=NewTRef}};   
		 {die, Reason} ->
		     {stop, Reason}
	     end,
    Branch.
		 
handle_sb_msg({_Ref, Id, _Msg}, #state{id=Id}=State) ->
    {noreply, State};
handle_sb_msg({_Ref, _OtherId, Msg}, #state{id=Id}=State) ->
    dl_softbus:bcast(agents, Id, Msg),
    {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% internal functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
start_countdown(Seconds) ->
    erlang:send_after(1000*Seconds, self(), do_record).

gen_dt_id(ChannelId) ->
    erlang:list_to_atom(erlang:atom_to_list(ChannelId) ++ "_dt").

do_collect_data(ChanData) ->
    case dl_conf_mgr:get_read_mfa(dl_ch_data:get_id(ChanData)) of
	{error, no_channel} ->
	    {die, no_channel};
	{{prologix,_,_},read,[Instr,Chan]} ->
	    do_read_prologix(Instr,Chan)
    end.

do_read_prologix(Instr, Chan) ->
    case gen_prologix:read(Instr,Chan) of
	{error, Reason} ->
	    Arg = [Chan, Reason],
	    lager:info("logger on channel ~p failed with reason ~p",Arg);
	Data ->
	    Msg = {nd, {Instr, Chan}, Data},
	    dl_softbus:bcast(agents,self(),Msg),
	    ok
    end.

%%%%%%%%%%%%%
%%% EUNIT %%%
%%%%%%%%%%%%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

id_test() ->
    ?assertEqual(test_dt, gen_dt_id(test)).

-endif.

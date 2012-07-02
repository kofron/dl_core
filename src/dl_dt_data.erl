%% @doc dl_dt_data is the data structure module associated with
%%      data taker processes.
%% @author jared kofron <jared.kofron@gmail.com>
-module(dl_dt_data).

%%%%%%%%%%%%%%%%%%%
%%% Core record %%%
%%%%%%%%%%%%%%%%%%%
-record(dl_dt_data,{
	  channel :: atom(),
	  interval :: integer(),
	  pid :: pid()
	 }).

-opaque dt_data() :: #dl_dt_data{}.
-export_type([dt_data/0]).

%%%%%%%%%%%
%%% API %%%
%%%%%%%%%%%
-export([new/0, fields/0, from_json/1]).
-export([get_pid/1, set_pid/2,
	 get_channel/1,set_channel/2,
	 get_interval/1,set_interval/2]).

%%%%%%%%%%%%%%%%%%%%%%%
%%% API definitions %%%
%%%%%%%%%%%%%%%%%%%%%%%
-spec new() -> dt_data().
new() ->
    #dl_dt_data{}.
    
-spec fields() -> [atom()].
fields() ->
    record_info(fields, dl_dt_data).

-spec from_json(ejson:json_object()) -> dt_data().
from_json(JS) ->
    new(). % NOO NOOO NNNNNOOOO

%%---------------------------------------------------------------------%%
%% Getters and setters                                                 %%
%%---------------------------------------------------------------------%%
-spec get_channel(dt_data()) -> atom().
get_channel(#dl_dt_data{channel=C}) ->
    C.

-spec get_interval(dt_data()) -> integer().
get_interval(#dl_dt_data{interval=I}) ->
    I.

-spec get_pid(dt_data()) -> pid().
get_pid(#dl_dt_data{pid=P}) ->
    P.

-spec set_channel(dt_data(),atom()) -> dt_data().
set_channel(#dl_dt_data{}=Dt,NewCh) when is_atom(NewCh) ->
    Dt#dl_dt_data{channel=NewCh}.

-spec set_interval(dt_data(),integer()) -> dt_data().
set_interval(#dl_dt_data{}=Dt,NewIval) when is_integer(NewIval) ->
    Dt#dl_dt_data{interval=NewIval}.

-spec set_pid(dt_data(),pid()) -> dt_data().
set_pid(#dl_dt_data{}=Dt,NewPid) when is_pid(NewPid) ->
    Dt#dl_dt_data{pid=NewPid}.

%%%%%%%%%%%%%%%
%%% Testing %%% 
%%%%%%%%%%%%%%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%% Test that the new() function gives us a new record.
new_rec_test() ->
    ?assertEqual(#dl_dt_data{},dl_dt_data:new()).    

%%% Set/get tests.
set_pid_test() ->
    N = new(),
    TestPid = erlang:list_to_pid("<0.0.0>"),
    Np = set_pid(N,TestPid),
    ?assertEqual(TestPid, get_pid(Np)).

set_interval_test() ->
    N = new(),
    Np = set_interval(N,10),
    ?assertEqual(10,get_interval(Np)).

set_channel_test() ->
    N = new(),
    Np = set_channel(N, achannel),
    ?assertEqual(achannel,get_channel(Np)).

set_all_test() ->
    N = new(),
    TestPid = erlang:list_to_pid("<0.0.0>"),
    Np = set_channel(N, achannel),
    Nq = set_interval(Np, 10),
    Nr = set_pid(Nq, TestPid),
    ?assertEqual(achannel,get_channel(Nr)),
    ?assertEqual(TestPid,get_pid(Nr)),
    ?assertEqual(10, get_interval(Nr)).

-endif.

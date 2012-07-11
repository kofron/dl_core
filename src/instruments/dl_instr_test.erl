-module(dl_instr_test).
-behaviour(gen_prologix).

-record(instr_st,{}).
-export([init/1,start_link/3,do_read/2,do_write/2]).

init(_Args) ->
    {ok, #instr_st{}}.

start_link(ID, EProID, GPIBAddr) ->
    gen_prologix:start_link(?MODULE, ID, EProID, GPIBAddr).

% Can return:
%% {data, Data, NewState}
%% {send, Binary, NewState}
%% {error, Reason, NewState}
%% {stop, NewState}.
do_read(Ch, StateData) ->
    {send, Ch, StateData}.

do_write(_A, _B) ->
    ok.

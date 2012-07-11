%% @doc the Hewlett-Packard HP8340B is a high frequency synthesized 
%%		sweeper controlled via GPIB.
%% @author jared kofron <jared.kofron@gmail.com>
%% @version 0.1a
-module(hp8340b).
-behavior(gen_prologix).

%%%%%%%%%%%
%%% API %%%
%%%%%%%%%%%
-export([do_read/2, do_write/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server api and callbacks %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([init/1,start_link/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% internal record defs %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(state, {}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server api and callbacks definitions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link(InstrumentID,EProID,GPIBAddress) ->
    gen_prologix:start_link(?MODULE, InstrumentID, EProID, GPIBAddress).

init(_Args) ->
	InitialState = #state{},
	{ok, InitialState}.

do_read(cw_freq, StateData) ->
    {send, <<"OPCW">>, StateData};
do_read(power_level, StateData) ->
    {send, <<"OPPL">>, StateData};
do_read(sweep_start_freq, StateData) ->
    {send, <<"OPFA">>, StateData};
do_read(sweep_stop_freq, StateData) ->
    {send, <<"OPFB">>, StateData};
do_read(sweep_time, StateData) ->
    {send, <<"OPST">>, StateData};
do_read(AnyOther, StateData) ->
    {error, {unknown_channel, AnyOther}, StateData}.

do_write(cw_freq, <<"disable">>, StateData) ->
    {send, [<<"RF0">>], StateData};
do_write(cw_freq, <<"enable">>, StateData) ->
    {send, [<<"RF1">>], StateData};
do_write(cw_freq, NewValue, StateData) ->
    {send, [<<"CW">>, NewValue, <<"MZ">>], StateData};
do_write(power_level, NewValue, StateData) ->
    {send, [<<"PL">>, NewValue, <<"DB">>], StateData};
do_write(sweep_start_freq, NewValue, StateData) ->
    {send, [<<"FA">>, NewValue, <<"MZ">>], StateData};
do_write(sweep_stop_freq, NewValue, StateData) ->
    {send, [<<"FB">>, NewValue, <<"MZ">>], StateData};
do_write(sweep_time, NewValue, StateData) ->
    {send, [<<"ST">>, NewValue, <<"MS">>], StateData};
do_write(AnyOther, _NewValue, StateData) ->
    {error, {unknown_channel, AnyOther}, StateData}.

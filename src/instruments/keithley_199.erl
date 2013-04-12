% keithley_199.erl
% @author jared kofron <jared.kofron@gmail.com>
% @doc A frontend module for the keithley 199 DMM.
-module(keithley_199).
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

do_read(dmm_input, StateData) ->
    {send, <<"R3T5X">>, StateData}.

do_write(_AnyChannel, _NewValue, StateData) ->
    {error, {unsupported_method, write}, StateData}.

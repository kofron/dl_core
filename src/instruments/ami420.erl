% ami420.erl
% @author jared kofron <jared.kofron@gmail.com>
% @doc A frontend module for the American Magnetics Power Supply Controller
% model 420.  Currently only supports reading current and voltage from the
% supply, as well as the coil constant.
-module(ami420).
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

do_read(supply_current, StateData) ->
    {send, <<"CURR:MAG?">>, StateData};
do_read(supply_voltage, StateData) ->
    {send, <<"VOLT:SUPP?">>, StateData};
do_read(coil_constant, StateData) ->
    {send, <<"COIL?">>, StateData};
do_read(magnetic_field, StateData) ->
    {send, <<"FIELD:MAG?">>, StateData};
do_read(ramp_state, StateData) ->
    {send, <<"STATE?">>, StateData};
do_read(persistent_switch, StateData) ->
    {send, <<"PSwitch?">>, StateData}.

do_write(_AnyChannel, _NewValue, StateData) ->
    {error, {unsupported_method, write}, StateData}.

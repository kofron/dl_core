-module(dsp7265).
-behavior(gen_prologix).

-export([do_read/2,do_write/3]).
-export([init/1, start_link/3]).
-record(state, {}).

start_link(InstrumentID, EproID, GPIBAddress) ->
    gen_prologix:start_link(?MODULE, InstrumentID, EproID, GPIBAddress).

init(_Args) ->
    InitialState = #state{},
    {ok, InitialState}.

do_read(x, State) ->
    {send, <<"X.">>, State};
do_read(y, State) ->
    {send, <<"Y.">>, State};
do_read(mag, State) ->
    {send, <<"MAG.">>, State};
do_read(phase, State) ->
    {send, <<"PHA.">>, State};
do_read(xy, State) ->
    {send, <<"XY.">>, State};
do_read(magphase, State) ->
    {send, <<"MP.">>, State}.
do_write(sensitivity, Value, State) ->
    {send, [<<"SEN ">>, Value], State};
do_write(gain, Value, State) ->
    {send, [<<"ACGAIN ">>, Value], State};
do_write(osc_amplitude, Value, State) ->
    {send, [<<"OA. ">>, Value], State};
do_write(osc_freq, Value, State) ->
    {send, [<<"OF. ">>, Value], State}.









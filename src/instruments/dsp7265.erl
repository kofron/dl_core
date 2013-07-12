-module(dsp7265).
-behavior(gen_prologix).

-export([do_read/2,do_write/3, do_parse/2]).
-export([init/1, start_link/3]).
-ifdef(TEST).
-export([data_output/1]).
-endif.

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
    {send, <<"MP.">>, State};
do_read(data_status, State) ->
    {send, <<"M">>, State};
do_read('curve.x', State) ->
    DataBit = data_output(x_out),
    {send_then_parse, [<<"DCB ">>,erlang:integer_to_list(DataBit)], State}.

do_write(sensitivity, Value, State) ->
    {send, [<<"SEN ">>, Value], State};
do_write(gain, Value, State) ->
    {send, [<<"ACGAIN ">>, Value], State};
do_write(osc_amplitude, Value, State) ->
    {send, [<<"OA. ">>, Value], State};
do_write(osc_freq, Value, State) ->
    {send, [<<"OF. ">>, Value], State};
do_write(data_curves, Value, State) ->
    CurveStr = erlang:integer_to_list(data_output(Value)),
    {send, [<<"CBD ">>, CurveStr], State};
do_write(take_data_register, _, State) ->
    {send, [<<"TD">>], State}.

do_parse(Data, State) ->
    {ok, parse_twos_complement(Data), State}.

parse_twos_complement(Bin) ->
    parse_twos_complement_acc(Bin, []).
parse_twos_complement_acc(<<>>, Acc) ->
    lists:reverse(Acc);
parse_twos_complement_acc(<<Value:16/binary,Rest/binary>>=Bin,Acc) ->
    DecodedInt = case is_positive(Value) of 
        true ->
            binary_to_16bit(Value);
        false ->
            decode_negative_value(Value)
        end,
    parse_twos_complement_acc(Rest,[DecodedInt|Acc]).

is_positive(<<0:1,_Rest/binary>>) ->
    true;
is_positive(_MSBIsSet) ->
    false.

decode_negative_value(Bin) ->
    Flipped = flip_bits(Bin),
    binary_to_16bit(Flipped).
flip_bits(Binary) ->
    << <<(B bxor 1):1>> || <<B:1>> <= Binary >>.

binary_to_16bit(<<Val:16/integer>>) ->
    Val.

data_output(Outputs) when is_list(Outputs) ->
    data_output_acc(Outputs, 0);
data_output(Output) when is_atom(Output) ->
    1 bsl data_output_table(Output).

data_output_acc([], Acc) ->
    Acc;
data_output_acc([O|Rest], Acc) ->
    data_output_acc(Rest, Acc + (1 bsl data_output_table(O))).

data_output_table(x_out) ->
    0;
data_output_table(y_out) ->
    1;
data_output_table(mag_out) ->
    2;
data_output_table(phase) ->
    3;
data_output_table(sensitivity) ->
    4;
data_output_table(adc1) ->
    5;
data_output_table(adc2) ->
    6;
data_output_table(adc3) ->
    7;
data_output_table(dac1) ->
    8;
data_output_table(dac2) ->
    9;
data_output_table(noise) ->
    10;
data_output_table(ratio) ->
    11;
data_output_table(log_ratio) ->
    12;
data_output_table(event) ->
    13;
data_output_table(ref_bits_0t15) ->
    14;
data_output_table(ref_bits_16t32) ->
    15.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

data_output_test() ->
    BitsDef = [{x_out, 1}, 
	      {y_out, 2},
	      {mag_out, 4},
	      {phase, 8},
	      {sensitivity, 16},
	      {adc1, 32},
	      {adc2, 64},
	      {adc3, 128},
	      {dac1, 256},
	      {dac2, 512},
	      {noise, 1024},
	      {ratio, 2048},
	      {log_ratio, 4096},
	      {event, 8192},
	      {ref_bits_0t15,16384},
	      {ref_bits_16t32,32768}],
    BitsMap = lists:map(fun({B,D}) ->
				{data_output(B),D}
			end,
			BitsDef),
    lists:map(fun({X,Y}) ->
		      ?assertEqual(X,Y)
	      end,
	      BitsMap).

use_case_test() ->
    Outputs = [x_out, y_out, sensitivity, adc1],
    ?assertEqual(data_output(Outputs), 51).

-endif.














    









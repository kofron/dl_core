%% @doc The Agilent E3634A is a triple output laboratory power supply.
%% @author jared kofron <jared.kofron@gmail.com>
%% @version 0.1a
-module(agilent_e3634a).
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

do_read('25V', StateData) ->
    {send, <<"APPL?">>, StateData};
do_read('50V', StateData) ->
    {send, <<"APPL?">>, StateData}.

do_write('25V', NewValue, StateData) ->
    Branch = case unpack_value(NewValue) of
		 {ok, <<"ON">>} ->
		     ToSend = ["VOLT:RANG P25V;:","OUTP:STAT ON"],
		     {send, ToSend, StateData};
		 {ok, <<"OFF">>} ->
		     ToSend = ["VOLT:RANG P25V;:","OUTP:STAT OFF"],
		     {send, ToSend, StateData};
		 {ok, Parsed} ->
		     ToSend = ["APPL ",Parsed],
		     {send, ToSend, StateData};
		 {error, Reason} ->
		     {error, Reason, StateData}
	     end,
    Branch;
do_write('50V', NewValue, StateData) ->
    Branch = case unpack_value(NewValue) of
		 {ok, <<"ON">>} ->
		     ToSend = ["VOLT:RANG P50V;:","OUTP:STAT ON"],
		     {send, ToSend, StateData};
		 {ok, <<"OFF">>} ->
		     ToSend = ["VOLT:RANG P50V;:","OUTP:STAT OFF"],
		     {send, ToSend, StateData};
		 {ok, Parsed} ->
		     ToSend = ["VOLT:RANG P50V;: APPL ",Parsed],
		     {send, ToSend, StateData};
		 {error, Reason} ->
		     {error, Reason, StateData}
	     end,
    Branch.

%%---------------------------------------------------------------------%%
%% @doc unpack_value converts a value passed in from the database 
%%---------------------------------------------------------------------%%
-spec unpack_value(binary()) -> 
			  {ok, binary()} | {error, term()}.
unpack_value(<<"enable">>) ->
    {ok, <<"ON">>};
unpack_value(<<"disable">>) ->
    {ok, <<"OFF">>};
unpack_value(Bin) ->
    case binary:match(Bin,<<",">>) of
	nomatch ->
	    parse_const_mode(Bin);
	_Match ->
	    parse_reg_mode(Bin)
    end.
	    
-spec parse_const_mode(binary()) -> binary().
parse_const_mode(B) ->
    case {is_v(B), is_c(B)} of
	{true, false} ->
	    {ok, [drop_v(B),", ",<<"MAX">>]};
	{false, true} ->
	    {ok, [<<"MAX">>,", ",drop_c(B)]};
        _AnyOther ->
	    {error, {badval, B}}
    end.

-spec parse_reg_mode(binary()) -> binary().
parse_reg_mode(B) ->
    [V,C] = binary:split(B,<<",">>),
    case {is_v(V), is_c(V), is_v(C), is_c(C)} of
	{true, false, false, true} -> % Voltage, Current
	    {ok, [drop_v(V),", ",drop_c(C)]};
	{false, true, false, true} -> % Current, Current
	    {error, {badval, B}};
	{false, true, true, false} -> % Current, Voltage
	    {ok, [drop_v(C),", ",drop_c(V)]};
	{true, false, true, false} -> % Voltage, Voltage
	    {error, {badval, B}} ;
	_Other -> % Nonsense
	    {error, {badval,B}}
    end.

-spec is_v(binary()) -> boolean().
is_v(B) ->
    binary:last(B) == $V.

-spec is_c(binary()) -> boolean().
is_c(B) ->
    binary:last(B) == $A.

-spec drop_v(binary()) -> binary().
drop_v(Bin) ->
    binary:replace(Bin,<<"V">>,<<>>).

-spec drop_c(binary()) -> binary().
drop_c(Bin) ->
    binary:replace(Bin,<<"A">>,<<>>).

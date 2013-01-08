% dl_hook.erl
% @doc dl hooks are like git hooks - they are functions that
%      get applied either before or after data is taken to transform
%      it in some way.  this could be conversion to JSON, or calibration,
%      or whatever.  a hook is defined as a function 
%      hook :: dl_data() -> dl_data()
%      such that the caller sees a transparent data source.
% @todo This entire module can actually be generated at runtime!
-module(dl_hooks).

%%%%%%%%%%%%%
%%% Types %%%
%%%%%%%%%%%%%
-type hook() :: atom().
-export_type([hook/0]).

%%%%%%%%%%%
%%% API %%%
%%%%%%%%%%%
-export([apply_hooks/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Calibration hooks %%%
%%%%%%%%%%%%%%%%%%%%%%%%%
-export([kjlc354_cal/1, 
	 linear_r_to_z/1,
	 force_positive/1,
	 cernox33122/1,
	 cernox43022/1,
	 cernox01912/1,
	 cernox01929/1,
	 cernox31305/1,
	 tm220/1,
	 precision_shunt/1,
	 lakeshore_hall_cal_80K/1]).

%%%%%%%%%%%%%%%%%%%%%%%
%%% Aesthetic hooks %%%
%%%%%%%%%%%%%%%%%%%%%%%
-export([strip_newline_chars/1]).

%%%%%%%%%%%%%%%%%%%%%%%
%%% API Definitions %%%
%%%%%%%%%%%%%%%%%%%%%%%
-spec apply_hooks(binary, dl_ch_data:ch_data()) -> 
			 dl_ch_data:ch_data().
apply_hooks(ChName, Data) ->
    case is_binary(dl_data:get_data(Data)) of
	true ->
	    D = dl_conf_mgr:channel_info(ChName),
	    Hooks = dl_ch_data:get_post_hooks(D),
	    do_apply_hooks(Data, Hooks);
	false ->
	    skip_processing(Data)
    end.

do_apply_hooks(Data, Hooks) ->
    case dl_data:get_code(Data) of
	ok ->
	    Raw = dl_data:get_data(Data),
	    Final = lists:foldl(fun(X,Acc) ->
					apply(dl_hooks, X, [Acc]) 
				end, Raw, Hooks),
	    dl_data:set_final(Data, Final);
	error ->
	    Raw = dl_data:get_data(Data),
	    Final = error_to_proplist(Raw),
	    dl_data:set_final(Data, Final)
    end.

%%%%%%%%%%%%%%%%%%%
%%% Definitions %%%
%%%%%%%%%%%%%%%%%%%
-spec force_positive(binary()) -> binary().
force_positive(<<"-",Rest/binary>>) ->
    A = <<"+">>,
    <<A/binary, Rest/binary>>;
force_positive(Other) ->
    Other.

-spec tm220(binary()) -> binary().
tm220(<<Val:15/binary,_Rest/binary>>) ->
    Raw = dl_util:binary_to_float(Val),
    P = math:pow(10,linear_interp(0.5880813592280791,Raw,-3)),
    erlang:list_to_binary([erlang:float_to_list(P), " Torr"]).

-spec precision_shunt(binary()) -> binary().
precision_shunt(<<Val:15/binary,_Rest/binary>>) ->
    Raw = dl_util:binary_to_float(Val),
    % Precision shunt is 50mV/20A
    R = 50.0e-03/20.0,
    P = linear_interp(1.0/R,Raw,0.0),
    erlang:list_to_binary([erlang:float_to_list(P), " A"]).

-spec lakeshore_hall_cal_80K(binary()) -> binary().
lakeshore_hall_cal_80K(<<Val:15/binary,_Rest/binary>>) ->
    Raw = dl_util:binary_to_float(Val),
    P = linear_interp(0.118*0.9991,Raw*1.0e3 + 0.7e-3,0.0),
    erlang:list_to_binary([erlang:float_to_list(P), " kG"]).
    
-spec kjlc354_cal(binary()) -> binary().
kjlc354_cal(<<Val:15/binary,_Rest/binary>>) ->
    Raw = dl_util:binary_to_float(Val),
    Cal = math:pow(10,Raw - 10),
    erlang:list_to_binary([erlang:float_to_list(Cal)," Torr"]).

-spec strip_newline_chars(binary()) -> binary().
strip_newline_chars(<<>>) ->
    <<>>;
strip_newline_chars(Bin) ->
    lager:debug("strip newline chars: ~p",[Bin]),
    case binary:last(Bin) of
	$\n ->
	    strip_newline_chars(binary:part(Bin,{0, byte_size(Bin) -1}));
	_AnyOther ->
	    Bin
    end.

-spec linear_r_to_z(binary()) -> binary().
linear_r_to_z(<<Val:15/binary,_Rest/binary>>) ->
    Raw = dl_util:binary_to_float(Val),
    Z = linear_interp(0.02460529,Raw,-29.12859597),
    erlang:list_to_binary([erlang:float_to_list(Z)," mm"]).

-spec cernox33122(binary()) -> binary().
cernox33122(<<Val:15/binary,_Rest/binary>>) ->
    Raw = dl_util:binary_to_float(Val),
    Bin = do_cernox_cal(Raw, cernox33122_points()),
    erlang:list_to_binary([erlang:float_to_list(Bin)," K"]).

-spec cernox43022(binary()) -> binary().
cernox43022(<<Val:15/binary,_Rest/binary>>) ->
    Raw = dl_util:binary_to_float(Val),
    Bin = do_cernox_cal(Raw, cernox43022_points()),
    erlang:list_to_binary([erlang:float_to_list(Bin)," K"]).

-spec cernox01912(binary()) -> binary().
cernox01912(<<Val:15/binary,_Rest/binary>>) ->
    Raw = dl_util:binary_to_float(Val),
    Bin = do_cernox_cal(Raw, cernox01912_points()),
    erlang:list_to_binary([erlang:float_to_list(Bin)," K"]).

-spec cernox01929(binary()) -> binary().
cernox01929(<<Val:15/binary,_Rest/binary>>) ->
    Raw = dl_util:binary_to_float(Val),
    Bin = do_cernox_cal(Raw, cernox01929_points()),
    erlang:list_to_binary([erlang:float_to_list(Bin)," K"]).

-spec cernox31305(binary()) -> binary().
cernox31305(<<Val:15/binary,_Rest/binary>>) ->
    Raw = dl_util:binary_to_float(Val),
    Bin = do_cernox_cal(Raw, cernox31305_points()),
    erlang:list_to_binary([erlang:float_to_list(Bin)," K"]).

%%%%%%%%%%%%%%%%
%%% Internal %%%
%%%%%%%%%%%%%%%%
-spec do_cernox_cal(float(),[{float(),float}]) -> float().
do_cernox_cal(Res, CalPts) ->
    LogRes = math:log(Res),
    Interval = [{_X1,_Y1},{_X2,_Y2}] = locate_interval(LogRes,CalPts),
    {M,B} = slope_intercept(Interval),
    LogT = linear_interp(M,LogRes,B),
    math:exp(LogT).

-spec slope_intercept([{float(),float()}]) -> {float(),float()}.
slope_intercept([{_X1,_Y1},{_X2,_Y2}]=Coords) ->
    M = slope(Coords),
    B = intercept(M,Coords),
    {M,B}.

-spec slope([{float(),float()}]) -> float().
slope([{X1,Y1},{X2,Y2}]) ->
    (Y2 - Y1)/(X2 - X1).

-spec intercept(float(), [{float(),float()}]) -> float().
intercept(M, [{X1,Y1},{_X2,_Y2}]) ->
    Y1 - M*X1.

-spec locate_interval(float(), [{float(),float()}]) -> [{float(),float()}].
% First a straight find.  If the value is between two X values, we found it.
locate_interval(Pt, [{X1,_}=A,{X2,_}=B|_Rest]) when Pt >= X1 andalso Pt < X2 ->
    [A,B];
% OK, if the value is above the highest bin, use the last two bins.
locate_interval(Pt, [{X1,_Y1}=A,{X2,_Y2}=B]) when Pt >= X2 andalso Pt >= X1 ->
    [A,B];
% If the value is smaller than the smallest bin and the next smallest, use the
% first two bins.
locate_interval(Pt, [{X1,_Y1}=A,{X2,_Y2}=B|_Rest]) when Pt =< X1 andalso Pt < X2 ->
    [A,B];
% If none of the previous clauses are true, drop A and continue.
locate_interval(Pt, [_Hd|Tail]) ->
    locate_interval(Pt, Tail).
    
-spec cernox33122_points() -> [{float(), float()}].
cernox33122_points() ->
    Raw = [{47.6,300}, % R was originally 57.6 here, but changed due to empirical error
	   {81.1,200},
	   {149,100},
	   {180,80},
	   {269,50},
	   {598,20}],
    lists:map(fun({X,Y}) ->
		      {math:log(X),math:log(Y)}
	      end,
	      Raw).

-spec cernox43022_points() -> [{float(), float()}].
cernox43022_points() ->
        Raw = [{68.6,300},
	   {248,78},
	   {3771,4.2}],
    lists:map(fun({X,Y}) ->
		      {math:log(X),math:log(Y)}
	      end,
	      Raw).

-spec cernox01912_points() -> [{float(), float()}].
cernox01912_points() ->
        Raw = [{45.5,297},
	   {167.5,77},
           {310.9,40},
           {318.2,39},
           {433.4,28}],
    lists:map(fun({X,Y}) ->
		      {math:log(X),math:log(Y)}
	      end,
	      Raw).

-spec cernox31305_points() -> [{float(), float()}].
cernox31305_points() ->
        Raw = [{62.8,300},
	       {186,78},
	       {4203,4.2}],
    lists:map(fun({X,Y}) ->
		      {math:log(X),math:log(Y)}
	      end,
	      Raw).

-spec cernox01929_points() -> [{float(), float()}].
cernox01929_points() ->
        Raw = [{45.5,297},
	       {187.5,77},
	       {440.9,30.5},
	       {1922,6.7},
	       {2249,5.9},
	       {3445,4.3},
	       {4611,3.5},
	       {6146,3},
	       {8338,2.5},
	       {11048,2.1},
	       {11352,2}],
    lists:map(fun({X,Y}) ->
		      {math:log(X),math:log(Y)}
	      end,
	      Raw).



-spec linear_interp(float(), float(), float()) -> float().
linear_interp(M, X, B) ->
    M*X + B.

-spec skip_processing(dl_data:dl_data()) -> dl_data:dl_data().
skip_processing(Data) ->
    Data.

error_to_proplist({K, {_K1, _V1}=V}) ->
    F = fun erlang:atom_to_binary/2,
    [{F(K,latin1), error_to_proplist(V)}];
error_to_proplist({K,V}) ->
    F = fun erlang:atom_to_binary/2,
    [{F(K,latin1),F(V,latin1)}].

%%%%%%%%%%%%%%%
%%% Testing %%% 
%%%%%%%%%%%%%%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

cernox33122_test() ->
    Delta = 1.0E-03,
    ?assert(do_cernox_cal(57.6,cernox33122_points()) - 300 < Delta),
    ?assert(do_cernox_cal(598,cernox33122_points()) - 20 < Delta).

-endif.

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
	 cernox01912/1]).

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
    D = dl_conf_mgr:channel_info(ChName),
    Hooks = dl_ch_data:get_post_hooks(D),
    do_apply_hooks(Data, Hooks).

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
    <<A/binary, Rest/binary>>.

-spec kjlc354_cal(binary()) -> binary().
kjlc354_cal(<<Val:15/binary,_Rest/binary>>) ->
    Raw = dl_util:binary_to_float(Val),
    Cal = math:pow(10,Raw - 10),
    erlang:list_to_binary([erlang:float_to_list(Cal)," Torr"]).

-spec strip_newline_chars(binary()) -> binary().
strip_newline_chars(<<>>) ->
    <<>>;
strip_newline_chars(Bin) ->
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
locate_interval(Pt, [{X1,_}=A,{X2,_}=B|_Rest]) when Pt >= X1 andalso Pt < X1 ->
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
    Raw = [{57.6,300},
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
    Raw = [{300,68.6},{78,248},{4.2,3771}],
    lists:map(fun({X,Y}) ->
		      {math:log(X),math:log(Y)}
	      end,
	      Raw).

-spec cernox01912_points() -> [{float(), float()}].
cernox01912_points() ->
    Raw = [{297,45.5},
	   {77,167.5},
           {40,310.9},
           {39,318.2},
           {28,433.4}],
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
    ?assert(do_cernox_cal(57.6,cernox33122_points()) - 300 < Delta),
    ?assert(do_cernox_cal(598,cernox33122_points()) - 20 < Delta).

-endif.

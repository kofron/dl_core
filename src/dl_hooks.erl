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
-export([kjlc354_cal/1]).

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
    {ok, D} = dl_conf_mgr:lookup(ChName),
    {ok, Hooks} = dl_ch_data:get_fields(post_hooks, D),
    case Hooks of
	[] ->
	    skip_processing(Data);
	SomeHooks ->
	    do_apply_hooks(Data, SomeHooks)
    end.

do_apply_hooks(Data, Hooks) ->
    case dl_data:get_code(Data) of
	ok ->
	    Raw = dl_data:get_data(Data),
	    Final = lists:foldl(fun(X,Acc) ->
					apply(dl_hook, X, [Acc]) 
				end, Raw, Hooks),
	    dl_data:set_final(Data, Final);
	error ->
	    skip_processing(Data)
    end.

%%%%%%%%%%%%%%%%%%%
%%% Definitions %%%
%%%%%%%%%%%%%%%%%%%
-spec kjlc354_cal(binary()) -> binary().
kjlc354_cal(<<Val:15/binary,_Rest/binary>>) ->
    Raw = dl_util:binary_to_float(Val),
    Cal = math:pow(10,Raw - 10),
    erlang:list_to_binary([erlang:float_to_list(Cal)," Torr"]).

-spec skip_processing(dl_data:dl_data()) -> dl_data:dl_data().
skip_processing(Data) ->
    Data.
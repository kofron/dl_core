%% @doc dripline_util contains functions that are used all over
%%		the code base as helpers.
%% @author jared kofron <jared.kofron@gmail.com>
-module(dl_util).
%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Timestamp functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([make_ts/0, node_name/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Data munging functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([binary_to_atom/1, binary_to_float/1, binary_to_integer/1]).

%%---------------------------------------------------------------------%%
%% @doc make_ts converts the current time as reported by the erlang VM
%%      into the external timestamp format (a string).
%% @end
%%---------------------------------------------------------------------%%
-spec make_ts() -> binary().
make_ts() ->
    LocalTime = calendar:univeral_time(),
    to_binary_ts(LocalTime).

-spec to_binary_ts(calendar:datetime()) -> binary().
to_binary_ts({{Y,M,D},{HH,MM,SS}}) ->
    FormStr = "~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
    L = lists:flatten(io_lib:format(FormStr,[Y,M,D,HH,MM,SS])),
    list_to_binary(L).

%%---------------------------------------------------------------------%%
%% @doc binary_to_atom simply converts a binary string into an atom.
%% @end
%%---------------------------------------------------------------------%%
-spec binary_to_atom(binary()) -> atom().
binary_to_atom(Binary) ->
	erlang:list_to_atom(erlang:binary_to_list(Binary)).

%%---------------------------------------------------------------------%%
%% @doc binary_to_float converts a binary string with a floating point
%%      value into that floating point value.
%%      e.g. <<"1.4E-2">> -> 0.014
%% @end
%%---------------------------------------------------------------------%%
-spec binary_to_float(binary()) -> float().
binary_to_float(Binary) ->
	erlang:list_to_float(erlang:binary_to_list(Binary)).

%%---------------------------------------------------------------------%%
%% @doc binary_to_integer converts a binary string with an integer
%%      value into that integer value.
%%      e.g. <<"150">> -> 150.
%% @end
%%---------------------------------------------------------------------%%
-spec binary_to_integer(binary()) -> integer().
binary_to_integer(Binary) ->
	erlang:list_to_integer(erlang:binary_to_list(Binary)).

%%---------------------------------------------------------------------%%
%% @doc node_name returns the name of the node that we are running on.
%%---------------------------------------------------------------------%%
-spec node_name() -> atom().
node_name() ->
    [_, N] = binary:split(erlang:atom_to_binary(node(),latin1),<<"@">>),
    binary_to_atom(N, latin1).

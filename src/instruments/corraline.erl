% corraline.erl
% A frontend for the corraline utility.
-module(corraline).
-behavior(gen_os_cmd).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_os_cmd callbacks %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([init/1,start_link/1]).
-export([base_cmd/0,process_args/2]).

%%%%%%%%%%%%%%%%%%%%%%
%%% internal state %%%
%%%%%%%%%%%%%%%%%%%%%%
-record(state, {}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% callback definitions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link(ID) ->
    gen_os_cmd:start_link(?MODULE,ID).

init(_Args) ->
    {ok, #state{}}.

base_cmd() ->
    "/home/grybka/powerline/corraline".

process_args(Args, _StateData) ->
    proplist_to_args(Args, []).

proplist_to_args([],Acc) ->
    lists:reverse(Acc);
proplist_to_args([{points, P}|T], Acc) ->
    Str = lists:concat(["-f ", P]),
    proplist_to_args(T,[Str | Acc]);
proplist_to_args([{events, N}|T], Acc) ->
    Str = lists:concat(["-n ", N]),
    proplist_to_args(T,[Str | Acc]);
proplist_to_args([{input, I}|T], Acc) ->
    Str = lists:concat(["-i",I]),
    proplist_to_args(T,[Str | Acc]).

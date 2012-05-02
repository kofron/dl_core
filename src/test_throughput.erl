-module(test_throughput).

-export([go/1]).

go(N) ->
    spawn(fun() ->  loop(N) end).

loop(N) ->
    dl_softbus:attach(agents),
    T0 = erlang:now(),
    do_loop(N),
    io:format("~p~n",[timer:now_diff(erlang:now(),T0)]).

do_loop(0) ->
    ok;
do_loop(N) ->
    Ref = dl_softbus:bcast(agents, thru_loop, test),
    ok = do_recv(Ref),
    do_loop(N-1).

do_recv(Ref) ->
    receive
	{dl_sb_msg, Ref, _From, _Msg} ->
	    ok;
	_AnyOther ->
	    do_recv(Ref)
    after 
	500 ->
	    timeout
    end.

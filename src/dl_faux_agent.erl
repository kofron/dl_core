% dl_faux_agent.erl
% fakes an agent that just echos messages it receives
% back onto the bus.
-module(dl_faux_agent).
-behavior(gen_dl_agent).

-record(state, {
	  id
}).

-export([start_link/2,init/1,handle_sb_msg/2, handle_info/2]).

start_link(ID, Args) ->
    gen_dl_agent:start_link(?MODULE, ID).

init([ID|_T]) ->
    {ok, #state{id=ID}}.

handle_sb_msg({_Ref, Id, _Msg}, #state{id=Id}=State) ->
    {noreply, State};
handle_sb_msg({_Ref, _OtherId, Msg}, #state{id=Id}=State) ->
    io:format("recvd mesg, echoing~n"),
    dl_softbus:bcast(agents, Id, Msg),
    {noreply, State}.

handle_info(Info, State) ->
    io:format("got info ~p~n",[Info]),
    {noreply, State}.

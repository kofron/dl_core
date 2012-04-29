% gen_dl_agent.erl
% the generic dripline agent behavior.  this behavior encapsulates an
% agent which, upon starting successfully, attaches to the dripline 
% softbus.
-module(gen_dl_agent).

% We are wrapping gen_server, and adding a callback for when handle_info
% gets a dripline softbus message.
-behavior(gen_server).

% The behavior info
-export([behaviour_info/1]).

-export([init/1]).

-export([handle_call/3,handle_cast/2,terminate/2,code_change/3,handle_info/2]).

% We use init/1 to register with gproc, and handle_sb_msg is the special
% handling function for dripline softbus messages.
behaviour_info(callbacks) ->
    [
     {init,1},
     {start_link,2},
     {handle_sb_msg,2}
    ];
behaviour_info(_) ->
    undefined.


% Links as part of a supervision tree.
init([_ID|_T]=Args) ->
    case ?MODULE:init(Args) of
	{ok, _StateData} ->
	    true = dl_softbus:attach(agents);
	StartFailed ->
	    StartFailed
    end.

handle_info({dl_sb_msg, Msg}, StateData) ->
    ?MODULE:handle_sb_msg(Msg, StateData).

handle_call(Call, From, StateData) ->
    ?MODULE:handle_call(Call, From, StateData).

handle_cast(Cast, StateData) ->
    ?MODULE:handle_cast(Cast, StateData).

code_change(Version, StateData, Extra) ->
    ?MODULE:code_change(Version, StateData, Extra).

terminate(Reason, StateData) ->
    true = dl_softbus:detach(agents),
    ?MODULE:terminate(Reason, StateData).

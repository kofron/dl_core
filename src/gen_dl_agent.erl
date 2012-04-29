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

-export([init/1,start_link/2]).
-export([handle_call/3,handle_cast/2,terminate/2,code_change/3,handle_info/2]).

-record(gen_state,{mod, mod_sd}).

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
start_link(CallbackMod, ID) ->
    gen_server:start_link({local, ID}, ?MODULE, [ID,CallbackMod], []).

init([ID,CallbackMod]=Args) ->
    case CallbackMod:init(Args) of
	{ok, ModStateData}=StartOK ->
	    dl_softbus:attach(agents),
	    StateData = #gen_state{
	      mod = CallbackMod,
	      mod_sd = ModStateData
	     },
	    {ok, StateData};
	StartFailed ->
	    StartFailed
    end.

handle_info({dl_sb_msg, Ref, Id, M}, #gen_state{mod=Mod, mod_sd=SD}=GSD) ->
    {noreply, NewModState} = Mod:handle_sb_msg({Ref,Id,M}, SD),
    {noreply, GSD#gen_state{mod_sd=NewModState}}.

handle_call(Call, From, StateData) ->
    ?MODULE:handle_call(Call, From, StateData).

handle_cast(Cast, StateData) ->
    ?MODULE:handle_cast(Cast, StateData).

code_change(Version, StateData, Extra) ->
    ?MODULE:code_change(Version, StateData, Extra).

terminate(Reason, #gen_state{mod=Mod, mod_sd=SD}) ->
    ok = dl_softbus:detach(agents),
    Mod:terminate(Reason, SD).

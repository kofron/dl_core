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
-export([call/2,cast/2]).
-export([handle_call/3,handle_cast/2,terminate/2,code_change/3,handle_info/2]).

-record(gen_state,{mod, mod_sd}).

% We use init/1 to register with gproc, and handle_sb_msg is the special
% handling function for dripline softbus messages.
behaviour_info(callbacks) ->
    [
     {init,1},
     {start_link,2},
     {handle_sb_msg,2},
     {handle_call,3},
     {handle_cast,2},
     {handle_info,2},
     {code_change,3},
     {terminate,2}
    ];
behaviour_info(_) ->
    undefined.

call(AgentRef, Call) ->
    gen_server:call(AgentRef, Call).

cast(AgentRef, Cast) ->
    gen_server:cast(AgentRef, Cast).

% Links as part of a supervision tree.
start_link(CallbackMod, ID) ->
    gen_server:start_link({local, ID}, ?MODULE, [ID,CallbackMod], []).

init([_ID,CallbackMod]=Args) ->
    case CallbackMod:init(Args) of
	{ok, ModStateData} ->
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

handle_call(Call, From, #gen_state{mod=Mod, mod_sd=SD}=GSD) ->
    case Mod:handle_call(Call, From, SD) of
	{reply, Reply, NewStateData} ->
	    {reply, Reply, GSD#gen_state{mod_sd=NewStateData}}
    end.

handle_cast(Cast, #gen_state{mod=Mod, mod_sd=SD}=GSD) ->
    case Mod:handle_cast(Cast, SD) of
	{noreply, NewStateData} ->
	    {noreply, GSD#gen_state{mod_sd=NewStateData}}
    end.

code_change(Version, #gen_state{mod=Mod, mod_sd=SD}=GSD, Extra) ->
    {ok, NewModSD} = Mod:code_change(Version, SD, Extra),
    {ok, GSD#gen_state{mod_sd=NewModSD}}.

terminate(Reason, #gen_state{mod=Mod, mod_sd=SD}) ->
    ok = dl_softbus:detach(agents),
    Mod:terminate(Reason, SD).

%% @doc dl_ch_data is a data structure module that encapsulates
%%		all of the data that dripline has about a particular channel.
%% @author jared kofron <jared.kofron@gmail.com>
-module(dl_ch_data).

%%%%%%%%%%%%%%%%%%%
%%% Core record %%%
%%%%%%%%%%%%%%%%%%%
-type ch_type() :: rtd85 | rtd91 | dmm_dc | dmm_ac.

-record(cd,{
	  id = <<>>:: binary(),
	  node = local :: local | node(),
	  instr = none :: binary(),
	  model = none :: atom(),
	  locator = none :: term(),
	  type = dmm_dc :: ch_type(),
	  post_hk = [] :: [dripline_hook:hook()]
	 }).

-opaque ch_data() :: #cd{}.
-export_type([ch_data/0]).

%%%%%%%%%%%
%%% API %%%
%%%%%%%%%%%
-export([new/0]).
-export([
	 get_id/1,get_node/1,get_instr/1,get_model/1,
	 get_locator/1,get_type/1,get_post_hooks/1
	]).
-export([
	 set_id/2,set_node/2,set_instr/2,set_model/2,
	 set_locator/2,set_type/2,add_post_hook/2
	]).

%%%%%%%%%%%%%%%%%%%%%%%
%%% API Definitions %%% 
%%%%%%%%%%%%%%%%%%%%%%%

%%---------------------------------------------------------------------%%
%% @doc new/0 returns a new channel data structure.
%% @end
%%---------------------------------------------------------------------%%
-spec new() -> record().
new() ->
    #cd{}.

%%---------------------------------------------------------------------%%
%% Getters and setters %%
%%---------------------------------------------------------------------%%
get_id(#cd{id=ID}) ->
    ID.

get_node(#cd{node=Nd}) ->
    Nd.

get_instr(#cd{instr=In}) ->
    In.

get_model(#cd{model=Mod}) ->
    Mod.

get_locator(#cd{locator=Loc}) ->
    Loc.

get_type(#cd{type=Tp}) ->
    Tp.

get_post_hooks(#cd{post_hk=PH}) ->
    PH.

set_id(NewID, #cd{}=Rec) ->
    Rec#cd{id=NewID}.

set_node(NewNode, #cd{}=Rec) ->
    Rec#cd{node=NewNode}.

set_instr(NewInstr, #cd{}=Rec) ->
    Rec#cd{instr=NewInstr}.

set_model(NewNode, #cd{}=Rec) ->
    Rec#cd{node=NewNode}.

set_locator(NewLocator, #cd{}=Rec) ->
    Rec#cd{locator=NewLocator}.

set_type(NewType, #cd{}=Rec) ->
    Rec#cd{type=NewType}.

add_post_hook(NewHook, #cd{post_hk=PH}=Rec) ->
    Rec#cd{post_hk=PH ++ [NewHook]}.


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
-export([new/0, fields/0]).
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
%% @doc fields/0 returns a list of the fields in the data structure 
%%      a la record_info.  Is this a terrible idea?  Maybe.
%% @end
%%---------------------------------------------------------------------%%
-spec fields() -> [atom()].
fields() ->
    record_info(fields, cd).

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

%%%%%%%%%%%%%%%
%%% Testing %%% 
%%%%%%%%%%%%%%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%% Test that the new() function gives us a new record.
new_rec_test() ->
    ?assertEqual(#cd{},dl_ch_data:new()).

add_post_hooks_test() ->
    Hooks = [a,b,c],
    D0 = dl_ch_data:new(),
    DF = lists:foldl(fun(X,Acc) ->
			     dl_ch_data:add_post_hook(X,Acc)
		     end,
		     D0,
		     Hooks),
    ?assertEqual(Hooks, dl_ch_data:get_post_hooks(DF)).
    
set_type_test() ->
    %% test default type
    ?assertEqual(dmm_dc,dl_ch_data:get_type(#cd{})),
    
    %% set and check
    Type = dmm_ac,
    DF = dl_ch_data:set_type(Type,#cd{}),
    ?assertEqual(Type, dl_ch_data:get_type(DF)).

set_locator_test() ->
    %% test default locator
    ?assertEqual(none, dl_ch_data:get_locator(#cd{})),
    
    %% set and check
    Locator = someloc,
    DF = dl_ch_data:set_locator(Locator, #cd{}),
    ?assertEqual(Locator, dl_ch_data:get_locator(DF)).
    

-endif.

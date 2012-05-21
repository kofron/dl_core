%% @doc dl_ch_data is a data structure module that encapsulates
%%		all of the data that dripline has about a particular channel.
%% @author jared kofron <jared.kofron@gmail.com>
-module(dl_ch_data).

%%%%%%%%%%%%%%%%%%%
%%% Core record %%%
%%%%%%%%%%%%%%%%%%%
-type ch_type() :: rtd85 | rtd91 | dmm_dc | dmm_ac.

-record(dl_ch_data,{
	  id = <<>>:: binary(),
	  node = local :: local | node(),
	  instr = none :: binary(),
	  model = none :: atom(),
	  locator = none :: term(),
	  type = dmm_dc :: ch_type(),
	  post_hk = [] :: [dripline_hook:hook()]
	 }).

-opaque ch_data() :: #dl_ch_data{}.
-export_type([ch_data/0]).

%%%%%%%%%%%
%%% API %%%
%%%%%%%%%%%
-export([new/0, fields/0, from_json/1]).
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
-spec new() -> ch_data().
new() ->
    #dl_ch_data{}.

%%---------------------------------------------------------------------%%
%% @doc from_json/1 returns a new channel data structure constructed by
%%      recursively parsing JSON until all fields that can be set are 
%%      set.
%% @end
%%---------------------------------------------------------------------%%
-spec from_json(ejson:json_object()) -> {ok, ch_data()} | {error, term()}.
from_json(JS) ->
    D = new(),
    do_from_json(props:drop(['_id','_rev','type'],JS),D).
do_from_json({[]},Acc) ->
    {ok, Acc};
do_from_json({[{<<"name">>,N}|T]},Acc) ->
    Name = erlang:binary_to_atom(N, latin1),
    do_from_json({T}, dl_ch_data:set_id(Name, Acc));
do_from_json({[{<<"instr">>,N}|T]},Acc) ->
    Name = erlang:binary_to_atom(N, latin1),
    do_from_json({T}, dl_ch_data:set_instr(Name, Acc));
do_from_json({[{<<"locator">>,N}|T]},Acc) ->
    Name = erlang:binary_to_atom(N, latin1),
    do_from_json({T}, dl_ch_data:set_locator(Name, Acc));
do_from_json({[{<<"type">>,N}|T]},Acc) ->
    Name = erlang:binary_to_atom(N, latin1),
    do_from_json({T}, dl_ch_data:set_type(Name, Acc));
do_from_json({[{<<"node">>,N}|T]},Acc) ->
    Name = erlang:binary_to_atom(N, latin1),
    do_from_json({T}, dl_ch_data:set_node(Name, Acc));
do_from_json({[{<<"post_hooks">>,N}|T]},Acc) ->
    Hooks = lists:map(fun(X) ->
			      erlang:binary_to_atom(X, latin1)
		      end,
		      N),
    NewData = lists:foldl(fun(X,DatAcc) ->
			     dl_ch_data:add_post_hook(X,DatAcc)
			  end,
			  Acc,
			  Hooks),
    do_from_json({T}, NewData);    
do_from_json({[{_Other,_}|T]},Acc) ->
    do_from_json({T}, Acc).
    
%%---------------------------------------------------------------------%%
%% @doc fields/0 returns a list of the fields in the data structure 
%%      a la record_info.  Is this a terrible idea?  Maybe.
%% @end
%%---------------------------------------------------------------------%%
-spec fields() -> [atom()].
fields() ->
    record_info(fields, dl_ch_data).

%%---------------------------------------------------------------------%%
%% Getters and setters %%
%%---------------------------------------------------------------------%%
get_id(#dl_ch_data{id=ID}) ->
    ID.

get_node(#dl_ch_data{node=Nd}) ->
    Nd.

get_instr(#dl_ch_data{instr=In}) ->
    In.

get_model(#dl_ch_data{model=Mod}) ->
    Mod.

get_locator(#dl_ch_data{locator=Loc}) ->
    Loc.

get_type(#dl_ch_data{type=Tp}) ->
    Tp.

get_post_hooks(#dl_ch_data{post_hk=PH}) ->
    PH.

set_id(NewID, #dl_ch_data{}=Rec) ->
    Rec#dl_ch_data{id=NewID}.

set_node(NewNode, #dl_ch_data{}=Rec) ->
    Rec#dl_ch_data{node=NewNode}.

set_instr(NewInstr, #dl_ch_data{}=Rec) ->
    Rec#dl_ch_data{instr=NewInstr}.

set_model(NewNode, #dl_ch_data{}=Rec) ->
    Rec#dl_ch_data{node=NewNode}.

set_locator(NewLocator, #dl_ch_data{}=Rec) ->
    Rec#dl_ch_data{locator=NewLocator}.

set_type(NewType, #dl_ch_data{}=Rec) ->
    Rec#dl_ch_data{type=NewType}.

add_post_hook(NewHook, #dl_ch_data{post_hk=PH}=Rec) ->
    Rec#dl_ch_data{post_hk=PH ++ [NewHook]}.

%%%%%%%%%%%%%%%
%%% Testing %%% 
%%%%%%%%%%%%%%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%% Test that the new() function gives us a new record.
new_rec_test() ->
    ?assertEqual(#dl_ch_data{},dl_ch_data:new()).

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
    ?assertEqual(dmm_dc,dl_ch_data:get_type(#dl_ch_data{})),
    
    %% set and check
    Type = dmm_ac,
    DF = dl_ch_data:set_type(Type,#dl_ch_data{}),
    ?assertEqual(Type, dl_ch_data:get_type(DF)).

set_locator_test() ->
    %% test default locator
    ?assertEqual(none, dl_ch_data:get_locator(#dl_ch_data{})),
    
    %% set and check
    Locator = someloc,
    DF = dl_ch_data:set_locator(Locator, #dl_ch_data{}),
    ?assertEqual(Locator, dl_ch_data:get_locator(DF)).
    

-endif.

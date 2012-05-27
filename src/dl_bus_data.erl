%% @doc dl_bus_data is another data structure module that encapsulates
%%      information about buses.  We need this to dynamically start buses
%%      along with instruments.
-module(dl_bus_data).

%%%%%%%%%%%%%
%%% Types %%%
%%%%%%%%%%%%%
-type id_type() :: atom().
-type mod_type() :: atom().
-type info_type() :: [{atom(), term()}].
-type node_type() :: atom().

%%%%%%%%%%%%%%%%%%%
%%% Core Record %%%
%%%%%%%%%%%%%%%%%%%
-record(dl_bus_data, {
	  id :: id_type(),
	  module :: mod_type(),
	  info :: info_type(),
	  node :: node_type()
	 }).

%%%%%%%%%%%
%%% API %%%
%%%%%%%%%%%
-export([new/0, fields/0, from_json/1]).
-export([set_id/2, get_id/1]).
-export([set_module/2, get_module/1]).
-export([set_info/2,get_info/1]).
-export([set_node/2,get_node/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Type aliases and exports %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-opaque dl_bus_data() :: #dl_bus_data{}.
-export_type([dl_bus_data/0]).

%%%%%%%%%%%%%%%%%%%%%%%
%%% API Definitions %%% 
%%%%%%%%%%%%%%%%%%%%%%%

%%---------------------------------------------------------------------%%
%% @doc new/0 returns a new bus data structure.
%% @end
%%---------------------------------------------------------------------%%
-spec new() -> dl_bus_data().
new() ->
    #dl_bus_data{
     id = none,
     module = none,
     info = [],
     node = local
    }.

%%---------------------------------------------------------------------%%
%% @doc fields/0 returns a list of the fields in the data structure 
%%      a la record_info.  Is this a terrible idea?  Maybe.
%% @end
%%---------------------------------------------------------------------%%
-spec fields() -> [atom()].
fields() ->
    record_info(fields, dl_bus_data).

%%---------------------------------------------------------------------%%
%% @doc from_json/1 returns a new bus data structure constructed 
%%      by recursively parsing JSON until all fields that can be set are 
%%      set.
%% @end
%%---------------------------------------------------------------------%%
-spec from_json(ejson:json_object()) -> {ok, dl_bus_data()} 
					    | {error, term()}.
from_json(JS) ->
    D = new(),
    do_from_json(props:drop(['_id','_rev','type'],JS),D).
do_from_json({[]},Acc) ->
    {ok, Acc};
do_from_json({[{<<"name">>,N}|T]},Acc) ->
    Name = erlang:binary_to_atom(N, latin1),
    do_from_json({T}, dl_bus_data:set_id(Name, Acc));
do_from_json({[{<<"module">>,N}|T]},Acc) ->
    Name = erlang:binary_to_atom(N, latin1),
    do_from_json({T}, dl_bus_data:set_module(Name, Acc));
do_from_json({[{<<"node">>,N}|T]},Acc) ->
    Node = erlang:binary_to_atom(N, latin1),
    do_from_json({T}, dl_bus_data:set_node(Node, Acc));
do_from_json({[{<<"info">>,N}|T]},Acc) ->
    {ok, Properties} = parse_info(N),
    do_from_json({T}, dl_bus_data:set_info(Properties, Acc));
do_from_json({[{_Other,_}|T]},Acc) ->
    do_from_json({T}, Acc).

-spec parse_info(ejson:json_object()) -> {ok, term()} | {error, term()}.
parse_info(JS) ->
    parse_info(JS, []).
parse_info({[]}, Acc) ->
    {ok, Acc};
parse_info({[{<<"port">>,V}|T]}, Acc) ->
    Port = erlang:list_to_integer(erlang:binary_to_list(V)),
    parse_info({T}, [{port, Port}|Acc]);
parse_info({[{<<"ip_address">>,V}|T]}, Acc) ->
    Digits = binary:split(V,<<".">>,[global]),
    Integers = lists:map(fun(X) ->
				 B = erlang:binary_to_list(X),
				 erlang:list_to_integer(B)
			 end,
			 Digits),
    Final = erlang:list_to_tuple(Integers),
    parse_info({T}, [{ip_addr, Final}|Acc]).

%%---------------------------------------------------------------------%%
%% @doc set_id/3 sets the id field of an dl_bus_data object.
%% @end
%%---------------------------------------------------------------------%%
-spec set_id(id_type(),dl_bus_data()) -> dl_bus_data().
set_id(A,D) when is_record(D,dl_bus_data) ->
	D#dl_bus_data{id=A}.

%%---------------------------------------------------------------------%%
%% @doc get_id/3 returns the id field of an dl_bus_data object.
%% @end
%%---------------------------------------------------------------------%%
-spec get_id(dl_bus_data()) -> id_type().
get_id(#dl_bus_data{id=Id}) ->
	Id.

%%---------------------------------------------------------------------%%
%% @doc set_module/2 sets the module field of an dl_bus_data object.
%% @end
%%---------------------------------------------------------------------%%
-spec set_module(mod_type(),dl_bus_data()) -> dl_bus_data().
set_module(A,D) when is_record(D,dl_bus_data) ->
	D#dl_bus_data{module=A}.

%%---------------------------------------------------------------------%%
%% @doc get_module/2 gets the module field of an dl_bus_data object.
%% @end
%%---------------------------------------------------------------------%%
-spec get_module(dl_bus_data()) -> mod_type().
get_module(#dl_bus_data{module=M}) ->
	M.

%%---------------------------------------------------------------------%%
%% @doc set_info/2 sets the info field of an dl_bus_data object.
%% @end
%%---------------------------------------------------------------------%%
-spec set_info(info_type(), dl_bus_data()) -> dl_bus_data().
set_info(A,D) when is_list(A), is_record(D,dl_bus_data) ->
	D#dl_bus_data{info=A};
set_info(A,D) when is_record(D,dl_bus_data) ->
	D#dl_bus_data{info=[A]}.

%%---------------------------------------------------------------------%%
%% @doc get_info/1 gets the info field of an dl_bus_data object.
%% @end
%%---------------------------------------------------------------------%%
-spec get_info(dl_bus_data()) -> info_type().
get_info(#dl_bus_data{info=S}) ->
	S.

%%---------------------------------------------------------------------%%
%% @doc set_node/2 sets the node field of an dl_node_data object.
%% @end
%%---------------------------------------------------------------------%%
-spec set_node(node_type(),dl_bus_data()) -> dl_bus_data().
set_node(A,D) when is_record(D,dl_bus_data) ->
	D#dl_bus_data{node=A}.

%%---------------------------------------------------------------------%%
%% @doc get_node/1 sets the model field of an dl_node_data object.
%% @end
%%---------------------------------------------------------------------%%
-spec get_node(dl_bus_data()) -> node_type().
get_node(#dl_bus_data{node=B}) ->
	B.

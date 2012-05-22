%% @doc dripline_ch_data is a data structure module that encapsulates
%%		all of the data that dripline has about a particular 
%%              instrument.
%% @author jared kofron <jared.kofron@gmail.com>
-module(dl_instr_data).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Type defs for module %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type id_type() :: binary().
-type model_type() :: atom().
-type support_type() :: atom().
-type bus_type() :: {atom(), atom(), integer() | atom()}.

%%%%%%%%%%%%%%%%%%%
%%% Core record %%%
%%%%%%%%%%%%%%%%%%%
-record(dl_instr_data,{
		id :: id_type(),
		model :: model_type(),
		supports :: [support_type()],
		bus :: bus_type()
	}).

%%%%%%%%%%%
%%% API %%%
%%%%%%%%%%%
-export([new/0, fields/0, from_json/1]).
-export([set_id/2,get_id/1]).
-export([set_model/2,get_model/1]).
-export([set_bus/2,get_bus/1]).
-export([set_supports/2,get_supports/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% types and type exports %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-opaque dl_instr_data() :: #dl_instr_data{}.
-export_type([dl_instr_data/0]).

%%%%%%%%%%%%%%%%%%%%%%%
%%% API Definitions %%% 
%%%%%%%%%%%%%%%%%%%%%%%

%%---------------------------------------------------------------------%%
%% @doc new/0 returns a new channel data structure.
%% @end
%%---------------------------------------------------------------------%%
-spec new() -> dl_instr_data().
new() ->
	#dl_instr_data{
		id = <<>>,
		model = none,
		supports = [],
		bus = {none, none, none}
	}.

%%---------------------------------------------------------------------%%
%% @doc fields/0 returns a list of the fields in the data structure 
%%      a la record_info.  Is this a terrible idea?  Maybe.
%% @end
%%---------------------------------------------------------------------%%
-spec fields() -> [atom()].
fields() ->
    record_info(fields, dl_instr_data).

%%---------------------------------------------------------------------%%
%% @doc set_id/3 sets the id field of an dl_instr_data object.
%% @end
%%---------------------------------------------------------------------%%
-spec set_id(id_type(),dl_instr_data()) -> dl_instr_data().
set_id(A,D) when is_record(D,dl_instr_data) ->
	D#dl_instr_data{id=A}.

%%---------------------------------------------------------------------%%
%% @doc get_id/3 returns the id field of an dl_instr_data object.
%% @end
%%---------------------------------------------------------------------%%
-spec get_id(dl_instr_data()) -> id_type().
get_id(#dl_instr_data{id=Id}) ->
	Id.

%%---------------------------------------------------------------------%%
%% @doc set_model/2 sets the model field of an dl_instr_data object.
%% @end
%%---------------------------------------------------------------------%%
-spec set_model(model_type(),dl_instr_data()) -> dl_instr_data().
set_model(A,D) when is_record(D,dl_instr_data) ->
	D#dl_instr_data{model=A}.

%%---------------------------------------------------------------------%%
%% @doc get_model/2 gets the model field of an dl_instr_data object.
%% @end
%%---------------------------------------------------------------------%%
-spec get_model(dl_instr_data()) -> model_type().
get_model(#dl_instr_data{model=M}) ->
	M.

%%---------------------------------------------------------------------%%
%% @doc set_supports/2 sets the supports field of an dl_instr_data object.
%% @end
%%---------------------------------------------------------------------%%
-spec set_supports(support_type() | [support_type()], dl_instr_data()) ->
		dl_instr_data().
set_supports(A,D) when is_list(A), is_record(D,dl_instr_data) ->
	D#dl_instr_data{supports=A};
set_supports(A,D) when is_record(D,dl_instr_data) ->
	D#dl_instr_data{supports=[A]}.

%%---------------------------------------------------------------------%%
%% @doc get_supports/1 gets the supports field of an dl_instr_data object.
%% @end
%%---------------------------------------------------------------------%%
-spec get_supports(dl_instr_data()) -> [support_type()].
get_supports(#dl_instr_data{supports=S}) ->
	S.

%%---------------------------------------------------------------------%%
%% @doc set_bus/2 sets the bus field of an dl_instr_data object.
%% @end
%%---------------------------------------------------------------------%%
-spec set_bus(bus_type(),dl_instr_data()) -> dl_instr_data().
set_bus(A,D) when is_record(D,dl_instr_data) ->
	D#dl_instr_data{bus=A}.

%%---------------------------------------------------------------------%%
%% @doc get_bus/1 sets the model field of an dl_instr_data object.
%% @end
%%---------------------------------------------------------------------%%
-spec get_bus(dl_instr_data()) -> bus_type().
get_bus(#dl_instr_data{bus=B}) ->
	B.

%%---------------------------------------------------------------------%%
%% @doc from_json/1 returns a new instrument data structure constructed 
%%      by recursively parsing JSON until all fields that can be set are 
%%      set.
%% @end
%%---------------------------------------------------------------------%%
-spec from_json(ejson:json_object()) -> {ok, dl_instr_data()} 
					    | {error, term()}.
from_json(JS) ->
    D = new(),
    do_from_json(props:drop(['_id','_rev','type'],JS),D).
do_from_json({[]},Acc) ->
    {ok, Acc};
do_from_json({[{<<"name">>,N}|T]},Acc) ->
    Name = erlang:binary_to_atom(N, latin1),
    do_from_json({T}, dl_instr_data:set_id(Name, Acc));
do_from_json({[{<<"instrument_model">>,N}|T]},Acc) ->
    Name = erlang:binary_to_atom(N, latin1),
    do_from_json({T}, dl_instr_data:set_model(Name, Acc));
do_from_json({[{<<"bus">>,N}|T]},Acc) ->
    Bus = parse_bus_from_string(N),
    do_from_json({T}, dl_instr_data:set_bus(Bus, Acc));
do_from_json({[{<<"supports">>,N}|T]},Acc) ->
    Names = lists:map(fun(X) ->
			      erlang:binary_to_atom(X, latin1)
		      end,
		      N),
    do_from_json({T}, dl_instr_data:set_supports(Names, Acc));
do_from_json({[{_Other,_}|T]},Acc) ->
    do_from_json({T}, Acc).

%%---------------------------------------------------------------------%%
%% @doc Buses are stored in the database as strings in the form
%%      'bus_name/bus_process:bus_addr', but internally they are tuples
%%      that look like {bus_name, bus_proc, bus_addr}.  This function 
%%      parses the string into the internal representation.
%% @end
%%---------------------------------------------------------------------%%
-spec parse_bus_from_string(binary()) -> bus_type().
parse_bus_from_string(Bin) ->
    [BusB,ProcB,AddrB] = binary:split(Bin,[<<"/">>,<<":">>],[global]),
    {Addr,[]} = string:to_integer(erlang:binary_to_list(AddrB)),
    {
      binary_to_atom(BusB,latin1),
      binary_to_atom(ProcB,latin1),
      Addr
    }.

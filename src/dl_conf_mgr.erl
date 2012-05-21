%% dl_conf_mgr.erl
%% @doc The dripline configuration manager module.  Uses
%%      Mnesia as a backend for storing configuration data.
-module(dl_conf_mgr).
-behavior(gen_dl_agent).

-record(state,{id}).

-export([start_link/2,
	 init/1,
	 handle_sb_msg/2,
	 handle_info/2,
	 handle_cast/2,
	 handle_call/3,
	 code_change/3,
	 terminate/2]).

%%%%%%%%%%%%%%%%%%%%%%%%
%%% Includes for QLC %%%
%%%%%%%%%%%%%%%%%%%%%%%%
-include_lib("stdlib/include/qlc.hrl").

%%%%%%%%%%%%%%%%%%%%%
%%% API Functions %%%
%%%%%%%%%%%%%%%%%%%%%
-export([local_channels/0,channel_info/1]).

%%%%%%%%%%%%%%%%%%%%%%%
%%% API Definitions %%%
%%%%%%%%%%%%%%%%%%%%%%%
local_channels() ->
    gen_dl_agent:call(?MODULE, local_ch).

channel_info(Ch) ->
    gen_dl_agent:call(?MODULE, {info, Ch}).

start_link(?MODULE, _Args) ->
    gen_dl_agent:start_link(?MODULE, ?MODULE).

init([ID|_T]) ->
    ok = create_mnesia_tables(),
    {ok, #state{id=ID}}.

%% Handle messages coming from the couchdb adapter.  These are going
%% to be mostly configuration changes, as that's what the adapter likes
%% to pump into the softbus.
handle_sb_msg({_Ref, dl_cdb_adapter, Msg}, #state{}=State) ->
    maybe_update_tables(Msg),
    {noreply, State}.

handle_info(_Info, StateData) ->
    {noreply, StateData}.

handle_call(local_ch, _From, StateData) ->
    {reply, get_local_chs(), StateData};
handle_call({info, Ch}, _From, StateData) ->
    Reply = case get_ch_data(Ch) of
		{ok, Data} ->
		    Data;
		{error, _Reason}=E ->
		    E
	    end,
    {reply, Reply, StateData}.

handle_cast(_Cast, StateData) ->
    {noreply, StateData}.

code_change(_Version, StateData, _Extra) ->
    {ok, StateData}.

terminate(_Reason, _StateData) ->
    ok.

%%%%%%%%%%%%%%%%
%%% Internal %%%
%%%%%%%%%%%%%%%%
-spec create_mnesia_tables() -> ok | term().
create_mnesia_tables() ->
    ok = create_ch_data_table().

-spec create_ch_data_table() -> ok | term().
create_ch_data_table() ->
    case mnesia:create_table(dl_ch_data,
			     [
			      {ram_copies, [node()]},
			      {attributes, dl_ch_data:fields()}
			     ]) of
	{atomic, ok} ->
	    ok;
	{aborted,{already_exists,dl_ch_data}} ->
	    ok;
	AnyOther ->
	    AnyOther
    end.

-spec get_local_chs() -> [atom()].
get_local_chs() ->
    Qs = qlc:q([Ch || Ch <- mnesia:table(dl_ch_data),
		      dl_ch_data:get_node(Ch) == local
	       ]),
    {atomic, Ans} = mnesia:transaction(fun() ->
					       qlc:e(Qs)
				       end),
    Ans.

-spec get_ch_data(atom()) -> {ok, dl_ch_data:ch_data()}
				 | {error, term()}.
get_ch_data(ChName) ->
    Qs = qlc:q([Ch || Ch <- mnesia:table(dl_ch_data),
		      dl_ch_data:get_id(Ch) == ChName
	       ]),
    {atomic, Ans} = mnesia:transaction(fun() ->
					       qlc:e(Qs)
				       end),
    case Ans of
	[] ->
	    {error, no_channel};
	[H] ->
	    {ok, H}
    end.

-spec maybe_update_tables(ejson:json_object()) -> ok.
maybe_update_tables(Msg) ->
    case props:get('doc.type',Msg) of
	<<"channel">> ->
	    update_channel_table(Msg);
	<<"instrument">> ->
	    update_instrument_table(Msg);
	Other ->
	    declare_nonsense(Msg)
    end.
	
-spec update_channel_table(ejson:json_object()) -> ok.
update_channel_table(Msg) ->	      
    Doc = props:get('doc', Msg),
    {ok, ChData} = dl_ch_data:from_json(Doc),
    add_or_update_channel(ChData).

-spec update_instrument_table(ejson:json_object()) -> ok.
update_instrument_table(Msg) ->
    ok.

-spec declare_nonsense(ejson:json_object()) -> ok.
declare_nonsense(Msg) ->
    ok.

-spec add_or_update_channel(dl_ch_data:ch_data()) -> ok.
add_or_update_channel(ChData) ->
    case get_ch_data(dl_ch_data:get_id(ChData)) of
	{error, no_channel} ->
	    lager:info("new channel recvd: ~p",[ChData]);
	{ok, _Ch} ->
	    lager:info("overwriting channel conf for channel: ~p",[ChData])
    end,
    F = fun() ->
		mnesia:write(ChData)
	end,
    mnesia:transaction(F).

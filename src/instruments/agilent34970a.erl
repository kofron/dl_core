%% @doc The Agilent 34970a is a mux unit with interchangable
%%      cards.  It is a slightly more complicated beast than
%%      usual because we need to actually cache some data instead
%%      of reading every time.  The reason for this is that the
%%      instrument uses an armature to perform its reading, and 
%%      this is mechanically slow - much slower than the max rate
%%      at which we can send packets to the instrument.  Therefore,
%%      we utilize the scanning feature of the instrument in order
%%      to keep errors to a minimum.
-module(agilent34970a).
-behavior(gen_prologix).

%%%%%%%%%%%
%%% API %%%
%%%%%%%%%%%
-export([do_read/2, do_write/3]).
-export([parse_instrument_reply/2, do_update_cache/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server api and callbacks %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([init/1,start_link/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% internal record defs %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(cache_v, {lastval, ts, ch_type}).
-record(state, {q, cache, ttl, last_upd, new_ch}).

-type channel_spec() :: {integer(), integer()}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server api and callbacks definitions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link(InstrumentID,EProID,GPIBAddress) ->
    gen_prologix:start_link(?MODULE, InstrumentID, EProID, GPIBAddress).

init(_Args) ->
    InitialState = #state{ttl=20,
			 last_upd=erlang:now(),
			 cache=dict:new(),
			 new_ch=false},
    {ok, setup_cmds([]), InitialState}.

do_read(Ch, #state{ttl=T, last_upd=L, cache=C}=SD) ->
    Dt = timer:now_diff(erlang:now(), L)/1000000, % Dt in seconds
    DecodedCh = raw_ch_to_spec(Ch),
    Branch = case channel_is_known(DecodedCh, C) of
		 {false, bad_channel} ->
		     %% The channel is not a valid identifier for the unit.
		     {error, bad_channel, SD};
		 {false, new_channel} ->
		     %% The channel is valid, but so far we have not taken
		     %% data on it.  We need to update the cache which now
		     %% includes the new channel.
		     ChType = munge_ch_type(Ch),
		     NewDict = dict:store(DecodedCh, 
					  #cache_v{ch_type = ChType}, 
					  C),
		     NewSD = SD#state{cache = NewDict,new_ch=true},
		     {update_cache, NewSD};
		 {true, data, #cache_v{lastval=_Ls, ts=_Ts}} when Dt > T ->
		     %% The channel is valid, but more time has passed than
		     %% the TTL for the unit.  We need to update the *existing*
		     %% cache.
		     {update_cache, SD#state{new_ch=false}};
		 {true, data, #cache_v{lastval=Ls, ts=Ts}} ->
		     %% The channel is valid and the cache is valid.  Just 
		     %% return the data.
		     {data, {Ls, Ts}, SD}
	     end,
    Branch.

do_write(_Ch, _Value, _StateData) ->
    {error, unimplemented}.

do_update_cache(#state{cache=C,new_ch=true}=StateData) ->
    ConfCmd = configuration_string(C),
    ScanCmd = sl_cmd(sl_from_cache(C)),
    TrigCmd = trig_cmd(),
    NCh = dict:size(C),
    Cmd = [<<"ABOR;:">>,
	   ConfCmd,
	   ScanCmd,
	   <<";:">>,
	   "FORM:READ:CHAN ON;",
	   ":FORM:READ:TIME ON;", 
	   ":FORM:READ:TIME:TYPE ABS;",
	   ":FORM:READ:UNIT ON;:",
	   TrigCmd,
	   {sleep, 1500},
	   io_lib:format("DATA:REMOVE? ~B",[NCh])
	  ],
    {ok, Cmd, StateData};
do_update_cache(#state{cache=C,new_ch=false}=StateData) ->
    NCh = dict:size(C),
    Cmd = [<<"*TRG">>,
	   {sleep,600},
	   io_lib:format("DATA:REMOVE? ~B",[NCh])
	  ],
    {ok, Cmd, StateData}.
parse_instrument_reply({error, tcp_timeout}, StateData) ->
    {ok, StateData};
parse_instrument_reply(Rply, #state{cache=C}=StateData) ->
    Result = parse_instrument_response(Rply),
    NewCache = refresh_cache(Result, C),
    {ok, StateData#state{cache=NewCache,last_upd=erlang:now()}}.

%%%%%%%%%%%%%%%%
%%% Internal %%%
%%%%%%%%%%%%%%%%
configuration_string(Cache) ->
    Chs = dict:fold(fun(Loc,#cache_v{ch_type=T},A) ->
			   [{Loc,T}|A]
		    end,
		    [],
		    Cache),
    configuration_string(Chs,[]).
configuration_string([],Acc) ->
    Acc;
configuration_string([{Loc,Type}|T],Acc) ->
    ChString = io_lib:format("~B",[channel_tuple_to_int(Loc)]),
    ConfStr = [
	       "CONF:",
	       chan_type_string(Type),
	       " ",
	       chan_opts_string(Type),
	       ",(@",
	       ChString,
	       ");:"
	      ],
    configuration_string(T,[ConfStr|Acc]).

chan_type_string(rtd85) ->
    "TEMP";
chan_type_string(rtd85_fourwire) ->
    "TEMP";
chan_type_string(rtd91) ->
    "TEMP";
chan_type_string(dmm_dc) ->
    "VOLT:DC";
chan_type_string(dmm_dc_mv) ->
    "VOLT:DC";
chan_type_string(dcc_ac) ->
    "VOLT:AC";
chan_type_string(cernox) ->
    "FRES";
chan_type_string(fourwire) ->
    "FRES".

chan_opts_string(rtd85) ->
    "RTD,85";
chan_opts_string(rtd85_fourwire) ->
    "RTD,85";
chan_opts_string(rtd91) ->
    "RTD,91";
chan_opts_string(dmm_dc) ->
    "10,0.001";
chan_opts_string(dmm_dc_mv) ->
    "100mv,AUTO";
chan_opts_string(dmm_ac) ->
    "10,0.001";
chan_opts_string(cernox) ->
    "AUTO";
chan_opts_string(fourwire) ->
    "AUTO".



-spec munge_ch_type(atom()) -> atom().
munge_ch_type(ChannelLocator) ->
    RegName = proplists:get_value(registered_name, erlang:process_info(self())),
    case dl_conf_mgr:channel_info(RegName, ChannelLocator) of
	{error, no_ch} ->
	    dmm_dc;
	ChInfo ->
	    dl_ch_data:get_type(ChInfo)
    end.

refresh_cache([],Cache) ->
    Cache;
refresh_cache([{Loc,#cache_v{lastval=V,ts=Ts}}|T],Cache) ->
    ValUpdater = fun(#cache_v{}=CV) ->
			 CV#cache_v{lastval=V,ts=Ts}
		 end,
    NewCache = dict:update(Loc,ValUpdater,Cache),
    refresh_cache(T,NewCache).    

-spec raw_ch_to_spec(atom()) -> channel_spec().
raw_ch_to_spec(ChannelAtom) ->
    LocatorString = erlang:atom_to_list(ChannelAtom),
    case io_lib:fread("{~u,~u}",LocatorString) of
	{ok, [CardNumber,ChannelNumber], []} ->
	    {CardNumber,ChannelNumber};
	_Error ->{error, bad_locator}
    end.

-spec channel_is_known(term(), dict()) -> boolean().
channel_is_known(Channel, Cache) ->
    case dict:is_key(Channel, Cache) of
	true -> % Channel is known.  Grab last data.
	    {true, data, fetch_cached_value(Channel, Cache)};
	false ->
	    case is_valid_channel(Channel) of
		true ->
		    {false, new_channel};
		false ->
		    {false, bad_channel}
	    end
    end.

-spec is_valid_channel({integer(), integer()}) -> boolean().
is_valid_channel(Term) ->
    true.

fetch_cached_value(Channel, Cache) ->
    dict:fetch(Channel, Cache).

setup_cmds([]) ->
    [
     "*CLS;",
     "*RST;",
     ":FORM:READ:CHAN ON;",
     ":FORM:READ:TIME ON;", 
     ":FORM:READ:TIME:TYPE ABS;",
     ":FORM:READ:UNIT ON"
    ].

trig_cmd() ->
    [
     "TRIG:SOURCE BUS",
     ";:",
     "TRIG:COUNT INFINITY",
     ";:",
     "INIT",
     ";"
     "*TRG"
    ].

sl_cmd(ScanList) ->    
    io_lib:format("ROUT:SCAN (@~s)",[ScanList]).

sl_from_cache(Cache) ->
    channel_spec_list_to_scan_string(dict:fetch_keys(Cache)).

%%---------------------------------------------------------------------%%
%% @doc channel_spec_to_int/1 takes a channel specifier 
%%		{CardNumber,ChannelNumber} and converts it to the data that the 
%%		switch unit uses for addressing, which is a single integer.
%% @end
%%---------------------------------------------------------------------%%
-spec channel_tuple_to_int(channel_spec()) -> integer().
channel_tuple_to_int({CardNumber,ChannelNumber}) ->
	100*CardNumber + ChannelNumber.

%%---------------------------------------------------------------------%%
%% @doc channel_spec_list_to_scan_string is a very fun function.  
%%		it takes a list of 34970a channel specs and produces a
%%		string that the agilent 34970a is expecting, which is of the form
%%		"101,105,301...".  the cool part is range detection.  the 
%%		instrument takes ranges in the form of 101:105, which means all
%%		channels between 1 and 5 on card 1.  this function will 
%%		automagically produce the appropriate range queries if ranges
%%		are found in the list of tuples.
%% @end
%%---------------------------------------------------------------------%%
-spec channel_spec_list_to_scan_string([channel_spec()]) -> string().
channel_spec_list_to_scan_string([]) ->
	"";
channel_spec_list_to_scan_string({_,_}=SingleTuple) ->
	channel_spec_list_to_scan_string([SingleTuple]);
channel_spec_list_to_scan_string(Tuples) ->
	IntChannels = [channel_tuple_to_int(Y) || Y <- Tuples],
	channel_ints_to_scan_string(lists:sort(IntChannels),false,[]).

%%---------------------------------------------------------------------%%
%% @doc channel_ints_to_scan_string is where the magic happens in terms
%%		of generating the scan list for the agilent 34970a.
%% @see channel_spec_list_to_scan_string
%% @end
%%---------------------------------------------------------------------%%
-spec channel_ints_to_scan_string([channel_spec()],atom(),string()) ->
		string().
channel_ints_to_scan_string([], false, Acc) ->
	lists:flatten(lists:reverse(Acc));
channel_ints_to_scan_string([], H0, Acc) ->
	Str = io_lib:format(":~p",[H0]), 
	lists:reverse([Str|Acc]);

channel_ints_to_scan_string([H1,H2|T],false,Acc) when H2 == (H1 + 1) ->
	Str = io_lib:format("~p",[H1]),
	channel_ints_to_scan_string(T,H2,[Str|Acc]);

channel_ints_to_scan_string([H|T],H0,Acc) when H == (H0 + 1) ->
	channel_ints_to_scan_string(T,H,Acc);

channel_ints_to_scan_string([S],false,Acc) ->
	Str = io_lib:format("~p",[S]),
	channel_ints_to_scan_string([],false,[Str|Acc]);

channel_ints_to_scan_string([H|T],false,Acc) ->
	Str = io_lib:format("~p",[H]),
	channel_ints_to_scan_string(T,false,[Str ++ ","|Acc]);

channel_ints_to_scan_string([_H|_T]=L,H0,Acc) ->
	Str = io_lib:format(":~p",[H0]),
	channel_ints_to_scan_string(L,false,[Str ++ ","|Acc]).

%%---------------------------------------------------------------------%%
%% @doc parse_instrument_response takes the response from the unit and 
%%      turns it into a list of {channel, new_cache_value} tuples.
%% @end
%%---------------------------------------------------------------------%%
parse_instrument_response(Bin) ->
    parse_instrument_response(Bin, []).
parse_instrument_response(<<"\n">>, Acc) ->
    Acc;
parse_instrument_response(<<",",Rest/binary>>, Acc) ->
    parse_instrument_response(Rest,Acc);
parse_instrument_response(<<V:152/bitstring,
			    ",",
			    Y:32/bitstring,
			    ",",
			    M:16/bitstring,
			    ",",
			    D:16/bitstring,
			    ",",
			    HH:16/bitstring,
			    ",",
			    MM:16/bitstring,
			    ",",
			    SS:16/bitstring,
			    ".",
			    _MS:24/bitstring,
			    ",",
			    Ch:24/bitstring,
			    Rest/binary>>, Acc) ->
    Locator = ch_spec_from_binary(Ch),
    Ts = <<Y/binary,"-",M/binary,"-",D/binary,
	   " ", 
	   HH/binary,":",MM/binary,":",SS/binary>>,
    R = #cache_v{
	  lastval = V,
	  ts = Ts
	 },
    parse_instrument_response(Rest,[{Locator,R}|Acc]).

ch_spec_from_binary(Bin) ->
    {I,[]} = string:to_integer(binary:bin_to_list(Bin)),
    {I div 100, I rem 100}.

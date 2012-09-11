%% dl_instr.erl
%% @doc Top level module for dl_instr.  Does things like stop and
%%      start instruments.
-module(dl_instr).
-export([start_instr/1, start_bus/1]).

%%---------------------------------------------------------------------%%
%% @doc Starts the bus modules to communicate
%%      with a given instrument.  
%% @end
%%---------------------------------------------------------------------%%
-spec start_bus(dl_instr_data:dl_instr_data()) -> ok.
start_bus(BsData) ->
    BsChSpec = case dl_bus_data:get_module(BsData) of
		   prologix ->
		       generate_prologix_cspec(BsData);
		   unix ->
		       generate_unix_os_cspec(BsData)
	       end,
    ok = start_bus_from_spec(BsChSpec).

%%---------------------------------------------------------------------%%
%% @doc Starts the instrument modules to communicate
%%      with a given instrument.  
%% @end
%%---------------------------------------------------------------------%%
-spec start_instr(dl_instr_data:dl_instr_data()) -> ok.
start_instr(InData) ->
    InChSpec = generate_instr_cspec(InData),
    case supervisor:start_child(dl_instr_sup, InChSpec) of
	{ok, _} ->
	    ok;
	{ok, _, _} ->
	    ok;
	{error, Reason} ->
	    lager:error("couldn't start child from ~p (~p)", [InData, Reason]),
	    ok
    end.
    
%%---------------------------------------------------------------------%%
%% @doc Generates an instrument child specification from an 
%%      instrument module.
%% @end
%%---------------------------------------------------------------------%%
-spec generate_instr_cspec(dl_instr_data:dl_instr_data()) -> 
				  supervisor:child_spec() | {error, term()}.
generate_instr_cspec(InData) ->
    Name = dl_instr_data:get_id(InData),
    Mod  = dl_instr_data:get_model(InData),
    Args = case dl_instr_data:get_bus(InData) of
	       {prologix, BusProc, BusAddr} ->
		   [Name, BusProc, BusAddr];
	       {unix, _BusProc, _BusAddr} ->
		   [Name]
	   end,
    {
      Name,
      {
	Mod,
	start_link,
	Args
      },
      permanent,
      5000,
      worker,
      [Mod]
    }.

-spec generate_prologix_cspec(dl_bus_data:dl_bus_data()) -> 
				     supervisor:child_spec() | {error, term()}.
generate_prologix_cspec(BusInfo) ->
    Extras = dl_bus_data:get_info(BusInfo),
    {
      dl_bus_data:get_id(BusInfo),
      {
	eprologix_cmdr,
	start_link,
	[
	 proplists:get_value(ip_addr, Extras),
	 proplists:get_value(port, Extras),
	 dl_bus_data:get_id(BusInfo)
	]
      },
      permanent,
      5000,
      worker,
      [eprologix_cmdr]
    }.

-spec generate_unix_os_cspec(dl_bus_data:dl_bus_data()) -> 
				     supervisor:child_spec() | {error, term()}.
generate_unix_os_cspec(_BsData) ->
    {none,{none,none,none},none,none,none,[unix]}.

-spec start_bus_from_spec(supervisor:child_spec()) -> ok | {error, term()}.
start_bus_from_spec({_N,{_,_,_},_Tm,_Tmo,_Role,[eprologix_cmdr]}=Ch) ->
    case supervisor:start_child(eprologix_sup, Ch) of
	{ok, _} ->
	    ok;
	{ok, _, _} ->
	    ok;
	{error, {already_started, _}}->
	    ok;
	{error, Reason} ->
	    lager:error("Couldn't start prologix bus: ~p",[Reason]),
	    ok
    end;
start_bus_from_spec({_N,{_,_,_},_Tm,_Tmo,_Role,[unix]}) ->
    ok.    

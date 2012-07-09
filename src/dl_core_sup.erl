
-module(dl_core_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Compiler = ?CHILD(dl_compiler, worker),
    
    DTSup = ?CHILD(dl_data_taker_sup, supervisor),

    SysMgr = {
      dl_sys,
      {
	dl_sys,
	start_link,
	[dl_sys,[]]
      },
      permanent,
      5000,
      worker,
      [dl_sys]
     },

    ConfMgr = {
      dl_conf_mgr,
      {
	dl_conf_mgr,
	start_link,
	[dl_conf_mgr,[]]
      },
      permanent,
      5000,
      worker,
      [dl_conf_mgr]
     },

    CdbAdapter = {
      dl_cdb_adapter,
      {
	dl_cdb_adapter,
	start_link,
	[dl_cdb_adapter,[]]
      },
      permanent,
      5000,
      worker,
      [dl_cdb_adapter]
     },

    Children = [Compiler, ConfMgr,CdbAdapter, SysMgr, DTSup],

    {ok, { {one_for_one, 5, 10}, Children} }.



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

    Children = [ConfMgr],

    {ok, { {one_for_one, 5, 10}, Children} }.



-module(dl_data_taker_sup).

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
    SuperStrategy = {simple_one_for_one, 5, 10},
    LoggerProcess = {
      dl_data_taker,
      {
	dl_data_taker,
	start_link,
	[]
      },
      transient,
      5000,
      worker,
      [dl_data_taker]
     },
    {ok, { SuperStrategy, [LoggerProcess] }}.

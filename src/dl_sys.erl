% dl_sys.erl
% @author jared kofron <jared.kofron@gmail.com>
% @doc Interface to 'system' functions such as starting loggers, so on and
% so forth.
-module(dl_sys).
-behavior(gen_dl_agent).

%%%%%%%%%%%
%%% API %%%
%%%%%%%%%%%
-export([current_loggers/0]).
-export([start_loggers/1]).
-export([stop_loggers/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server api and callbacks %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, handle_sb_msg/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% internal server state %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(state, {}).

%%%%%%%%%%%%%%%%%%%%%%%
%%% API definitions %%%
%%%%%%%%%%%%%%%%%%%%%%%
-spec current_loggers() -> [atom()].
current_loggers() ->
    dl_conf_mgr:running_loggers().

-spec start_loggers([atom()]) -> [{atom(), atom() | {atom(), atom()}}].
start_loggers(Loggers) ->
    gen_dl_agent:call(?MODULE, {st_lg, Loggers}).

-spec stop_loggers([atom()]) -> [{atom(), atom() | {atom(), atom()}}].
stop_loggers(Loggers) ->
    gen_dl_agent:call(?MODULE, {sp_lg, Loggers}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server API and callback definitions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link(?MODULE, _Args) ->
    gen_dl_agent:start_link(?MODULE, ?MODULE).

init(_Args) ->
    {ok, #state{}}.

handle_call({st_lg, Lgs}, _From, State) ->
    Reply = do_start_loggers(Lgs, []),
    {reply, Reply, State};
handle_call({sp_lg, Lgs}, _From, State) ->
    Reply = do_stop_loggers(Lgs, []),
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_sb_msg(_Msg, #state{}=State) ->
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% internal functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
do_start_loggers([],Acc) ->
    Acc;
do_start_loggers([Lg|R], Acc) ->
    LgDt = dl_conf_mgr:logger_info(Lg),
    case LgDt of
	{error, no_logger}=Er ->
	    do_start_loggers(R, [{Lg, Er}|Acc]);
	_Data ->
	    Res = start_logger(LgDt),
	    do_start_loggers(R, [{Lg, Res}|Acc])
    end.

start_logger(LoggerData) ->
    ChName = dl_dt_data:get_channel(LoggerData),
    Ival = dl_dt_data:get_interval(LoggerData),
    case supervisor:start_child(dl_data_taker_sup, [dl_conf_mgr:channel_info(ChName), Ival]) of
	{ok, _Pid} ->
	    ok;
	{error, _Reason}=E ->
	    E
    end.

do_stop_loggers([], Acc) ->
    Acc;
do_stop_loggers([Lg|R], Acc) ->
    LgDt = dl_conf_mgr:logger_info(Lg),
    case LgDt of
	{error, no_logger}=Er ->
	    do_stop_loggers(R, [{Lg, Er}|Acc]);
	_Data ->
	    Res = stop_logger(LgDt),
	    do_stop_loggers(R, [{Lg, Res}|Acc])
    end.

stop_logger(LgDt) ->
    Pid = dl_dt_data:get_pid(LgDt),
    supervisor:terminate_child(dl_data_taker_sup, Pid).


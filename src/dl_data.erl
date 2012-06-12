%% dl_data.erl
%% dl_data is a data structure module that provides a common data
%% structure for data being returned by instrument modules.  it contains
%% response codes (error, ok, etc), data fields, timestamps, and whatever
%% else we might need in the future.
%% @author jared kofron <jared.kofron@gmail.com>
-module(dl_data).

-type dl_data_data_type() :: binary().
-type dl_data_code_type() :: error | ok.
-type dl_data_ts_type() :: binary().

-record(dl_data,{
	  code,
	  data,
	  final,
	  ts
}).

-opaque dl_data() :: #dl_data{}.
-export_type([dl_data/0]).

-export([new/0, from_prologix/1]).
-export([get_data/1,set_data/2,get_final/1,set_final/2]).
-export([get_code/1,set_code/2]).
-export([get_ts/1,set_ts/2]).

%%%%%%%%%%%%%%%%%%%%%%%
%%% API Definitions %%% 
%%%%%%%%%%%%%%%%%%%%%%%

%%---------------------------------------------------------------------%%
%% @doc new/0 returns a new dl_data structure.
%% @end
%%---------------------------------------------------------------------%%
-spec new() -> dl_data().
new() ->
    #dl_data{code=none,data=none,ts=none,final=none}.

%%---------------------------------------------------------------------%%
%% @doc from_prologix/1 returns a dl_data structure with values loaded
%%      according to the response from a gen_prologix device.
%% @end
%%---------------------------------------------------------------------%%
-spec from_prologix(term()) -> dl_data().
from_prologix(Result) ->
    N = new(),
    case Result of
	{error, Reason} ->
	    N#dl_data{code=error,
		      data=Reason,
		      ts=dl_util:make_ts()};
	{Data, Ts} ->
	    N#dl_data{code=ok,
		      data=Data,
		      ts=Ts}
    end.

%%---------------------------------------------------------------------%%
%% @doc get_data extracts the data field from a dl_data record.
%% @end
%%---------------------------------------------------------------------%%
-spec get_data(dl_data()) -> dl_data_data_type().
get_data(#dl_data{data=D}) ->
    D.

%%---------------------------------------------------------------------%%
%% @doc set_data sets the data field in a dl_data record
%% @end
%%---------------------------------------------------------------------%%
-spec set_data(dl_data(), dl_data_data_type()) -> dl_data().
set_data(DD, D) when is_record(DD, dl_data) ->
    DD#dl_data{data=D}.

%%---------------------------------------------------------------------%%
%% @doc get_final extracts the final data field from a dl_data record.
%% @end
%%---------------------------------------------------------------------%%
-spec get_final(dl_data()) -> dl_data_data_type().
get_final(#dl_data{final=none,data=D}) ->
    D;
get_final(#dl_data{final=F,data=_D}) ->
    F.

%%---------------------------------------------------------------------%%
%% @doc set_final sets the final data (post-hook) field in 
%%      a dl_data record
%% @end
%%---------------------------------------------------------------------%%
-spec set_final(dl_data(), dl_data_data_type()) -> dl_data().
set_final(DD, F) when is_record(DD, dl_data) ->
    DD#dl_data{final=F}.

%%---------------------------------------------------------------------%%
%% @doc set_code sets the status code field in a dl_data record
%% @end
%%---------------------------------------------------------------------%%
-spec set_code(dl_data(), dl_data_code_type()) -> dl_data().
set_code(DD, C) when is_record(DD, dl_data) ->
    DD#dl_data{code=C}.

%%---------------------------------------------------------------------%%
%% @doc get_code extracts the status code from a dl_data record
%% @end
%%---------------------------------------------------------------------%%
-spec get_code(dl_data()) -> dl_data_code_type().
get_code(#dl_data{code=C}) ->
    C.

%%---------------------------------------------------------------------%%
%% @doc get_ts extracts the timestamp from a dl_data record
%% @end
%%---------------------------------------------------------------------%%
-spec get_ts(dl_data()) -> dl_data_ts_type().
get_ts(#dl_data{ts=T}) ->
    T.

%%---------------------------------------------------------------------%%
%% @doc set_ts sets the timestamp in a dl_data record
%% @end
%%---------------------------------------------------------------------%%
-spec set_ts(dl_data(), dl_data_ts_type()) -> dl_data().
set_ts(DD, TS) when is_record(DD, dl_data) ->
    DD#dl_data{ts=TS}.

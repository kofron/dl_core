% cdb_json_verifier.erl
% this module is responsible for decoding and verifying incoming JSON
% from the couchdb interface.  the steps are:
%	1) JSX-powered parsing/semantic analysis (provided by cdb_json_analyzer module)
%	2) conversion to internal format that conf_mgr can recognize.
-module(cdb_json_verifier).
-export([verify/1]).

-spec verify(jsx:json_term()) -> {ok, req_data:req_data()} | {error, term()}.
verify(JSON) ->
	try cdb_json_analyzer:analyze(JSON) of
		{incomplete, _F} ->
			{error, incomplete_parse};
		Result ->
			Result
	catch
		Class:Except  ->
			{error, {Class, Except}}
	end.

%%%%%%%%%%%%%
%%% EUNIT %%%
%%%%%%%%%%%%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

basic_get_test() ->
	T = get_json_txt_0(),
	{Res, Dt} = cdb_json_verifier:verify(T),
	[
		?assertEqual(ok, Res),
		?assertEqual(get, req_data:verb(Dt)),
		?assertEqual(lo_cw_freq, req_data:ep(Dt))
	].

basic_set_test() ->
	T = set_json_txt_0(),
	{Res, Dt} = cdb_json_verifier:verify(T),
	[
		?assertEqual(ok, Res),
		?assertEqual(set, req_data:verb(Dt)),
		?assertEqual(lo_cw_freq, req_data:ep(Dt)),
		?assertEqual(586.0, req_data:req_val(Dt))
	].

get_json_txt_0() ->
	<<"{\"_id\": \"00316868759d473598112f9bc6b05118\",
   		\"_rev\": \"2-162e22297b3e247be0b5ed161465ef7e\",
   		\"command\": {
       		\"get\": \"lo_cw_freq\"
   		},
   		\"type\": \"command\",
   		\"result\": \"ok\",
		\"timestamp\": \"2013-03-12 15:34:44\",
   		\"final\": \"ok\"}">>.

set_json_txt_0() ->
	<<"{\"_id\": \"00316868759d473598112f9bc6b05118\",
   		\"_rev\": \"2-162e22297b3e247be0b5ed161465ef7e\",
   		\"command\": {
       		\"set\": \"lo_cw_freq\",
	        \"value\": 586.0
   		},
   		\"type\": \"command\",
   		\"result\": \"ok\",
		\"timestamp\": \"2013-03-12 15:34:44\",
   		\"final\": \"ok\"}">>.
-endif.
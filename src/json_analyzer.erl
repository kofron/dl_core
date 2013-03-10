-module(json_analyzer).
-export([init/1, handle_event/2]).

-record(imed, {
		act = none :: dl_types:act_type(),
		chn = none :: dl_types:chn_type(),
		id  = none :: dl_types:id_type(),
		tmo = none :: dl_types:tmo_type(),
		node = none :: dl_types:node_type()
	}).

-record(state, {
		key_tree = [],
		im_st :: #imed{},
		res = ok :: dl_types:succ_type(),
		ers = [dl_types:error_tuple()]
	}).

init([]) ->
	#state{
		key_tree = [], 
		im_st = #imed{},
		res = ok,
		ers = []
	}.

handle_event(start_object, State) ->
	State;
handle_event(end_object, #state{key_tree=[_K|R]}=State) ->
	State#state{key_tree=R};
handle_event(start_array, State) ->
	State;
handle_event(end_array, State) ->
	State;
handle_event({key, <<"get">>=K}, #state{key_tree=T}=State) ->
	State#state{key_tree=[K|T]};
handle_event({key, <<"set">>=K}, #state{key_tree=T}=State) ->
	State#state{key_tree=[K|T]};
handle_event({key, <<"value">>=K}, #state{key_tree=T}=State) ->
	State#state{key_tree=[K|T]};
handle_event({key, <<"id">>=K}, #state{key_tree=T}=State) ->
	State#state{key_tree=[K|T]};
handle_event({key, <<"timeout">>=K}, #state{key_tree=T}=State) ->
	State#state{key_tree=[K|T]};
handle_event({key, K}, #state{key_tree=T}=State) ->
	State#state{key_tree=[K|T]};
handle_event(end_json, #state{res=ok, im_st=Im}) ->
	{ok, Im};
handle_event(end_json, #state{res=error, ers=Ers}) ->
	{error, Ers};
% For all of the data callbacks below, the head of the key tree
% is the key that the value is associated with.
handle_event({string, S}, 
			#state{
				key_tree=[K|_R],
				ers = E,
				im_st=#imed{act=none}=I
			}=State) when K == <<"get">>; K == <<"set">> ->
	case parse_endpoint(S) of
		{ok, Node, EP} ->
			State#state{im_st=#imed{act=K, chn=EP, node=Node}};
		{ok, EP} ->
			State#state{im_st=#imed{act=K, chn=EP}};
		{error, Err} ->
			State#state{res = error, ers=[Err|E], im_st=I}
	end;
handle_event({string, _Str}, 
			#state{
				key_tree=[K|_R], 
				im_st=#imed{act=A}=I, ers=E
			}=State) when K == <<"get">>; K == <<"set">> ->
	Err = {ambiguous, {verbs, {A, K}}},
	State#state{im_st=I#imed{act=get}, res = error, ers=[Err|E]};
handle_event({_NotString, V}, 
			#state{
				key_tree=[<<"get">>|_R],
				im_st=#imed{act=none}=I,
				ers = E
			}=State) ->
	Err = {bad_value, [{field, get}, {value, V}]},
	State#state{im_st=I, res = error, ers = [Err|E]};
handle_event({integer, _Int}, State) ->
	State;
handle_event({float, _Float}, State) ->
	State;
handle_event({literal, _L}, State) ->
	State.

%%%%%%%%%%%%%%%%
%%% Internal %%%
%%%%%%%%%%%%%%%%
parse_endpoint(S) ->
	case endpoint:parse(S) of
		[{node, N}, {ep, E}] ->
			{ok, N, E};
		[{ep, E}] ->
			{ok, E};
		{fail, Rsn} ->
			{error, Rsn};
		{_Prt, R, {{line, _L}, {column, C}}} ->
			BadChar = binary:at(S, C),
			{error, [{bad_char, BadChar}, {at, R}]}
	end.

%%%%%%%%%%%%%
%%% EUNIT %%%
%%%%%%%%%%%%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

basic_get_test() ->
	DC = jsx:decoder(json_analyzer, [], []),
	T = <<"{\"get\": \"abc\"}">>,
	{Res, IMed} = DC(T),
	[
		?assertEqual(Res, ok),
		?assertEqual(IMed#imed.act, <<"get">>),
		?assertEqual(IMed#imed.chn, <<"abc">>)
	].
get_integer_channel_test() ->
	DC = jsx:decoder(json_analyzer, [], []),
	T = <<"{\"get\": 1}">>,
	{Res, IMed} = DC(T),
	Err = {bad_value, [{field, get}, {value, 1}]},
	[
		?assertEqual(Res, error),
		?assertEqual(IMed, [Err])
	].

get_multiple_verb_test() ->
	DC = jsx:decoder(json_analyzer, [], []),
	T = <<"{\"get\": \"abc\", \"set\": \"arg\"}">>,
	{Res, IMed} = DC(T),
	Err = {ambiguous, {verbs, {<<"get">>, <<"set">>}}},
	[
		?assertEqual(Res, error),
		?assertEqual(IMed, [Err])
	].

get_node_chn_test() ->
	DC = jsx:decoder(json_analyzer, [], []),
	T = <<"{\"get\": \"abc.def/ghi_jkl\"}">>,
	{Res, IMed} = DC(T),
	[
		?assertEqual(Res, ok),
		?assertEqual(IMed#imed.node, <<"abc.def">>),
		?assertEqual(IMed#imed.chn, <<"ghi_jkl">>),
		?assertEqual(IMed#imed.act, <<"get">>)
	].

-endif.
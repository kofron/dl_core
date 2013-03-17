-module(cdb_json_analyzer).
-export([init/1, handle_event/2, analyze/1]).

-record(imed, {
		act = none :: dl_types:act_type(),
		chn = none :: dl_types:chn_type(),
		id  = none :: dl_types:id_type(),
		tmo = none :: dl_types:tmo_type(),
		node = none :: dl_types:node_type(),
		val = none :: dl_types:req_val_t()
	}).

-record(state, {
		key_tree = [],
		im_st :: #imed{},
		res = ok :: dl_types:succ_type(),
		ers = [dl_types:error_tuple()],
		in_cmd = false
	}).

init([]) ->
	#state{
		key_tree = [], 
		im_st = #imed{},
		res = ok,
		ers = []
	}.

analyze(JSON) ->
	case (jsx:decoder(?MODULE, [], []))(JSON) of
		{ok, Im} ->
			imed_to_req_data(Im);
		AnyOther ->
			AnyOther
	end.

handle_event(start_object, State) ->
	State;
handle_event(end_object, #state{in_cmd=true,key_tree=[_K|R]}=State) ->
	State#state{key_tree=R,in_cmd=false};
handle_event(end_object, #state{key_tree=[_K|R]}=State) ->
	State#state{key_tree=R};
handle_event(end_object, #state{key_tree=[]}=State) ->
	State#state{key_tree=[]};
handle_event(start_array, State) ->
	State;
handle_event(end_array, State) ->
	State;
handle_event({key, <<"command">>}, #state{}=State) ->
	State#state{in_cmd=true};
handle_event({key, <<"get">>=K}, #state{in_cmd=true,key_tree=T}=State) ->
	State#state{key_tree=[K|T]};
handle_event({key, <<"get">>=K}, #state{ers=E,key_tree=[H|_T]=Tr}=State) ->
	Err = {error, {bad_tree_level, {H, <<"get">>}}},
	State#state{res=error, ers=[Err|E], key_tree=[K|Tr]};
handle_event({key, <<"set">>=K}, #state{in_cmd=true, key_tree=T}=State) ->
	State#state{key_tree=[K|T]};
handle_event({key, <<"set">>=K}, #state{ers=E,key_tree=[H|_T]=Tr}=State) ->
	Err = {error, {bad_tree_level, {H, <<"set">>}}},
	State#state{res=error, ers=[Err|E], key_tree=[K|Tr]};
handle_event({key, <<"value">>=K}, #state{in_cmd=true, key_tree=T}=State) ->
	State#state{key_tree=[K|T]};
handle_event({key, <<"value">>=K}, #state{ers=E,key_tree=[H|_T]=Tr}=State) ->
	Err = {error, {bad_tree_level, {H, <<"value">>}}},
	State#state{res=error, ers=[Err|E], key_tree=[K|Tr]};
handle_event({key, <<"timeout">>=K}, #state{in_cmd=true, key_tree=T}=State) ->
	State#state{key_tree=[K|T]};
handle_event({key, <<"timeout">>=K}, #state{ers=E,key_tree=[H|_T]=Tr}=State) ->
	Err = {error, {bad_tree_level, {H, <<"timeout">>}}},
	State#state{res=error, ers=[Err|E], key_tree=[K|Tr]};
handle_event({key, K}, #state{key_tree=T}=State) ->
	State#state{key_tree=[K|T]};
handle_event(end_json, #state{res=ok, im_st=Im}) ->
	{ok, Im};
handle_event(end_json, #state{res=error, ers=Ers}) ->
	{error, Ers};
% For all of the data callbacks below, the head of the key tree
% is the key that the value is associated with.

% Handle gets and sets.
handle_event({string, S}, 
			#state{
				key_tree=[K|_R],
				ers = E,
				im_st=#imed{act=none}=I
			}=State) when K == <<"get">>; K == <<"set">> ->
	case parse_endpoint(S) of 
		{ok, Node, EP} ->
			State#state{im_st=I#imed{act=K, chn=EP, node=Node}};
		{ok, EP} ->
			State#state{im_st=I#imed{act=K, chn=EP}};
		{error, Err} ->
			State#state{res = error, ers=[Err|E], im_st=I}
	end;
handle_event({_Type, V}, #state{key_tree=[<<"value">>|_R], 
								im_st=#imed{}=I}=State) ->
	State#state{im_st=I#imed{val=V}};
handle_event({_NotString, V},  
			#state{
				key_tree=[K|_R],
				im_st=#imed{act=none}=I,
				ers = E
			}=State) when K == <<"get">>; K == <<"set">> ->
	Err = {bad_value, [{field, K}, {value, V}]},
	State#state{im_st=I, res = error, ers = [Err|E]};
handle_event({string, _Str}, 
			#state{
				key_tree=[K|_R], 
				im_st=#imed{act=A}=I, ers=E
			}=State) when K == <<"get">>; K == <<"set">> ->
	Err = {ambiguous, {verbs, {A, K}}},
	State#state{im_st=I#imed{act=get}, res = error, ers=[Err|E]};

% Handle the couchdb _id field and bind it to the resulting data structure.
handle_event({string, Str}, 
			#state{
				key_tree=[<<"_id">>|_R], 
				im_st=#imed{}=I
			}=State) ->
	State#state{im_st=I#imed{id=Str}};

% Throw all others out
handle_event({integer, _Int}, State) ->
	State;
handle_event({float, _Float}, State) ->
	State;
handle_event({literal, _L}, State) ->
	State;
handle_event({_Type, _Value}, State) ->
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

imed_to_req_data(#imed{act=A,chn=C,id=I,val=V}) ->
	Dt0 = req_data:new(),
	Dt1 = req_data:set_ep(Dt0, binary_to_atom(C, latin1)),
	Dt2 = req_data:set_verb(Dt1, binary_to_atom(A, latin1)),
	Dt3 = req_data:set_id(Dt2, I),
	Dt4 = req_data:set_req_val(Dt3, V),
	{ok, Dt4}.

%%%%%%%%%%%%%
%%% EUNIT %%%
%%%%%%%%%%%%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

basic_get_test() ->
	DC = jsx:decoder(cdb_json_analyzer, [], []),
	T = <<"{\"get\": \"abc\"}">>,
	{Res, IMed} = DC(T),
	[
		?assertEqual(Res, ok),
		?assertEqual(IMed#imed.act, <<"get">>),
		?assertEqual(IMed#imed.chn, <<"abc">>)
	].
get_integer_channel_test() ->
	DC = jsx:decoder(cdb_json_analyzer, [], []),
	T = <<"{\"get\": 1}">>,
	{Res, IMed} = DC(T),
	Err = {bad_value, [{field, <<"get">>}, {value, 1}]},
	[
		?assertEqual(error, Res),
		?assertEqual([Err], IMed)
	].

get_multiple_verb_test() ->
	DC = jsx:decoder(cdb_json_analyzer, [], []),
	T = <<"{\"get\": \"abc\", \"set\": \"arg\"}">>,
	{Res, IMed} = DC(T),
	Err1 = {ambiguous, {verbs, {<<"get">>, <<"set">>}}},
	Err2 = {error, {bad_tree_level, {<<"get">>, <<"set">>}}},
	[
		?assertEqual(error, Res),
		?assertEqual([Err1,Err2], IMed)
	].

get_node_chn_test() ->
	DC = jsx:decoder(cdb_json_analyzer, [], []),
	T = <<"{\"get\": \"abc.def/ghi_jkl\"}">>,
	{Res, IMed} = DC(T),
	[
		?assertEqual(Res, ok),
		?assertEqual(IMed#imed.node, <<"abc.def">>),
		?assertEqual(IMed#imed.chn, <<"ghi_jkl">>),
		?assertEqual(IMed#imed.act, <<"get">>)
	].

-endif.
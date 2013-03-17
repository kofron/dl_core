% req_data
% workhorse data structure that encapsulates all information about
% a given request.
-module(req_data).
-export([new/0]).

% getters/setters
-export([ep/1,set_ep/2,
		req_val/1,set_req_val/2,
		verb/1,set_verb/2,
		id/1, set_id/2]).

-record(rqdt, {
		verb :: dl_types:verb_t(),
		ep :: dl_types:ep_t(),
		req_val :: dl_types:req_val_t(),
		id :: dl_types:req_id_t()
	}).

-opaque req_data() :: #rqdt{}.
-export_types([rqdt/0]).

-spec new() -> req_data().
new() ->
	#rqdt{}.

-spec verb(req_data()) -> dl_types:verb_t().
verb(#rqdt{verb=V}) ->
	V.

-spec set_verb(req_data(), dl_types:verb_t()) -> req_data().
set_verb(#rqdt{}=Rq, Verb) ->
	Rq#rqdt{verb=Verb}.

-spec ep(req_data()) -> dl_types:ep_t().
ep(#rqdt{ep=Ep}) ->
	Ep.

-spec set_ep(req_data(), dl_types:ep_t()) -> req_data().
set_ep(#rqdt{}=Rq, EP) ->
	Rq#rqdt{ep=EP}.

-spec req_val(req_data()) -> dl_types:req_val_t().
req_val(#rqdt{req_val=RV}) ->
	RV.

-spec set_req_val(req_data(), dl_types:req_val_t()) -> req_data().
set_req_val(#rqdt{}=Rq, RV) ->
	Rq#rqdt{req_val=RV}.

-spec id(req_data()) -> dl_types:req_id_t().
id(#rqdt{id=Id}) ->
	Id.

-spec set_id(req_data(), dl_types:req_id_t()) -> req_data().
set_id(#rqdt{}=Rq, ID) ->
	Rq#rqdt{id=ID}.
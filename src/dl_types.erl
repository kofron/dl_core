-module(dl_types).
-export_type([
		error_tuple/0,
		verb_t/0,
		ep_t/0,
		req_val_t/0,
		req_id_t/0
	]).

-type error_tuple() :: {error, term()}.
-type verb_t() :: get | set.
-type ep_t() :: atom().
-type req_val_t() :: string() | integer() | float().
-type req_id_t() :: binary().
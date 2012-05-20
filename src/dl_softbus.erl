% dl_softbus.erl
% dl_softbus provides the "software bus" that dripline uses to broadcast
% messages to dl_agents.   
-module(dl_softbus).

-export([attach/1,detach/1,bcast/3]).

-type group_name_type() :: atom().
-type softbus_msg_type() :: atom().
-type group_member_type() :: atom().

% Attach an agent to the bus.  Once attached, any message sent to any
% group that the agent is a member of will show up in its mailbox.
-spec attach(group_name_type()) -> ok | {error, regd}.
attach(GroupName) ->
    gproc:reg({p, l, GroupName}).

% Detach an agent from the bus.
-spec detach(group_name_type()) -> ok.
detach(GroupName) ->
    case gproc:unreg({p, l, GroupName}) of
	true ->
	    ok;
	false ->
	    false
    end.

% Send a softbus message to all members of a certain group.
-spec bcast(group_name_type(), group_member_type(), softbus_msg_type()) ->
		   ok | {error, nogrp}.
bcast(GroupName, Sndr, Msg) ->
    Ref = make_ref(),
    FullMsg = {dl_sb_msg, Ref, Sndr, Msg},
    FullMsg = gproc:send({p, l, GroupName}, FullMsg),
    Ref.

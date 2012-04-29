% dl_faux_agent.erl
% fakes an agent that just echos messages it receives
% back onto the bus.
-module(dl_faux_agent).
-behavior(gen_dl_agent).


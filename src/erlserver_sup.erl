-module(erlserver_sup).

-behaviour(supervisor).

-export([init/1]).
-export([start_link/0]).

-define(SERVER_NAME, ?MODULE).
-define(CHILD(C, Type), {C, {C, start_link, []}, temporary, 3000, Type, [C]}).


start_link() ->
	supervisor:start_link({local, ?SERVER_NAME}, ?MODULE, []).

init([]) ->
	AgentServer = ?CHILD(agent_server, supervisor),
	%PlayerServer = ?CHILD(player_server, supervisor),
	RestartStrategy = {one_for_one, 0, 1},
	{ok, {RestartStrategy, [AgentServer]}}.

-module(player_server).
-behaviour(supervisor).

-export([init/1]).
-export([start_link/0]).

-define(CHILD(C, Type), {C, {C, start_link, []}, permanent, 2000, Type, [C]}).
-define(SERVER_NAME, ?MODULE).

start_link() ->
	supervisor:start_link({local, ?SERVER_NAME}, ?MODULE, []).

init([]) ->
	PlayerChild = ?CHILD(player_sup, worker),
	Strategy = {one_for_one, 0, 1},
	{ok, {Strategy, [PlayerChild]}}.

-module(agent_server).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER_NAME, ?MODULE).

start_link() ->
	supervisor:start_link({local, ?SERVER_NAME}, ?MODULE, []).

init([]) ->
	start_network(),
	{ok, {{one_for_one, 0, 1}, []}}.

start_network() ->
	ok = application:start(ranch),
	ranch:start_listener(conn, 1000, ranch_tcp, [{port, 6159}], agent, []).

-module(player_server).
-behaviour(supervisor).

-export([init/1]).
-export([start_link/0]).

-define(CHILD(C, Type, StartArg), {C, {C, start_link, StartArg}, permanent, 2000, Type, [C]}).
-define(SERVER_NAME, ?MODULE).

start_link() ->
	supervisor:start_link({local, ?SERVER_NAME}, ?MODULE, []).

init([]) ->
  %{ok, Database} = application:get_env(mongodb_server, dbname),
  Database = <<"test">>,
  PlayerChild = ?CHILD(player_sup, supervisor, []),
  PlayerManagerChild = ?CHILD(player_manager, worker, []),
  MongodbChild = ?CHILD(mongodb_server, worker, [Database]),
  Strategy = {one_for_one, 0, 1},
  {ok, {Strategy, [MongodbChild, PlayerChild, PlayerManagerChild]}}.

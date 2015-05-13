-module(player_sup).

-behaviour(supervisor).

-define(SERVER_NAME, ?MODULE).
-define(CHILD(C, Type), {C, {C, start_link, []}, temporary, brutal_kill, Type, [C]}).

%% API
-export([start_link/0, start_child/1]).
%% Callbacks
-export([init/1]).


%% =============================
%% API 
%% =============================
start_link() ->
	supervisor:start_link({local, ?SERVER_NAME}, ?MODULE, []).

start_child([PlayerId, AgentPid]) ->
	supervisor:start_child(?SERVER_NAME, [PlayerId, AgentPid]).

%% =============================
%% Callbacks
%% =============================
init([]) ->
	Childs = [?CHILD(player, worker)],
	Strategy = {simple_one_for_one, 0, 1},
	{ok, {Strategy, Childs}}.

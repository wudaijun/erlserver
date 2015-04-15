-module(player).

-behaviour(gen_server).

%% API
-export([start_link/2]).
%% Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(state, {player_id, agent_pid}).

%% =========================
%% API
%% =========================
start_link(PlayerId, AgentPid) ->
	gen_server:start_link(?MODULE, [PlayerId, AgentPid], []).


%% =========================
%% Callbacks
%% =========================
init([PlayerId, AgentPid]) ->
	{ok, #state{player_id=PlayerId, agent_pid=AgentPid}}.

handle_call(_Msg, _From, State) ->
	{reply, ok, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(_Reason, _State) ->
	ok.

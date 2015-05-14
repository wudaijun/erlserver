-module(player).

-behaviour(gen_server).

%% API
-export([start_link/3, save/1]).
%% Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(state, {player_id}).

%% =========================
%% API
%% =========================
start_link(LoginWay, PlayerId,  Param) ->
  gen_server:start_link(?MODULE, [LoginWay, PlayerId, Param], []).


%% =========================
%% Callbacks
%% =========================
init([_LoginWay, PlayerId, _Param]) ->
  lager:info("--- new player [~p]~n", [PlayerId]),
  player_model:init(PlayerId), % 加载数据
  {ok, #state{player_id=PlayerId}}.

handle_call(_Msg, _From, State) ->
  {reply, ok, State}.

handle_cast(save, State) ->
  player_model:save(State#state.player_id),
  {noreply, State};

handle_cast({packet, M, F, A}, State) ->
  Res = apply(M, F, [A]),
  lager:info("--- res to client: ~p~n", [Res]),
  PlayerId = State#state.player_id,
  AgentPid = player_manager:get_agentpid(PlayerId),
  agent:send(AgentPid, Res),
  {noreply, State};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.

%% ========================
%% API 
%% ========================
save(PlayerPid) ->
  gen_server:cast(PlayerPid, save).

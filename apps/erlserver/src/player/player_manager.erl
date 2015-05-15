%%% =======================================
%%% @author wudaijun 2015-5-13
%%% @doc player_manager管理所有player进程
%%%      主要负责:
%%%         1. player的定时落地
%%%         2. 维护 PlayerId -> player进程 agent进程 三元组
%%% @end
%%% ========================================
-module(player_manager).

-behaviour(gen_server).

% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

% API exported
-export([start_link/0,
        get_playerpid/1,
        set_playerpid/2,
        get_agentpid/1,
        set_agentpid/2,
        player_login/3,
        player_register/3,
        connect/2,
        disconnect/2]).

-record(state, {}).
-record(item, {player_id, agent_pid, player_pid}).

% 落地时间间隔 30s
-define(SAVE_TIME_SPAN, 30*1000).

% ============================
% gen_server callbacks
% ============================

init([]) ->
  init_ets(),
  init_timer(),
  {ok, #state{}}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_call(_Req, _From, State) ->
  {reply, ok, State}.

handle_info(save_time, State) ->
  lager:info("--- save time, do saving ...~n"),
  do_save(),
  {noreply, State};

handle_info(_Info, State) ->
  {norely, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

% =================================
% API
% =================================

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_playerpid(PlayerId) ->
  case ets:lookup(?MODULE, PlayerId) of
    [] ->
      undefined;
    [Item] ->
      Item#item.player_pid
  end.

set_playerpid(PlayerId, PlayerPid) ->
  case ets:lookup(?MODULE, PlayerId) of
    [] ->
      ets:insert(?MODULE, #item{player_id=PlayerId, agent_pid=undefined, player_pid=PlayerPid});
    [Item] ->
      ets:insert(?MODULE, Item#item{player_pid=PlayerPid})
  end.

get_agentpid(PlayerId) ->
  case ets:lookup(?MODULE, PlayerId) of
    [] ->
      undefined;
    [Item] ->
      Item#item.agent_pid
  end.

set_agentpid(PlayerId, AgentPid) ->
  case ets:lookup(?MODULE, PlayerId) of
    [] ->
      ets:insert(?MODULE, #item{player_id=PlayerId, agent_pid=AgentPid, player_pid=undefined});
    [Item] ->
      ets:insert(?MODULE, Item#item{agent_pid=AgentPid})
  end.

% 玩家新连接到达
connect(PlayerId, AgentPid) ->
  case get_agentpid(PlayerId) of
    AgentPid ->
      ok;
    undefined ->
      set_agentpid(PlayerId, AgentPid);
    OldAgentPid -> % 踢掉之前的连接
      lager:info("--- kickout old connection"),
      agent:kickout(OldAgentPid),
      set_agentpid(PlayerId, AgentPid)
  end,
  ok.

% 玩家断线
disconnect(PlayerId, AgentPid) ->
  case get_agentpid(PlayerId) of
    AgentPid ->
      lager:info("--- reset agent to undefined"),
      set_agentpid(PlayerId, undefined);
    OtherAgent ->
      lager:info("--- I am ~p, cur agent is ~p", [AgentPid, OtherAgent]),
      do_nothing
  end,
  ok.

% 玩家登录
player_login(PlayerId, AgentPid, Param) ->
  case get_playerpid(PlayerId) of
    undefined ->
      {ok, PlayerPid} = player_sup:start_child(login, PlayerId, Param),
      set_playerpid(PlayerId, PlayerPid);
    PlayerPid -> 
      case process_info(PlayerPid) of
        undefined ->
          {ok, PlayerPid} = player_sup:start_child(login, PlayerId, Param),
          set_playerpid(PlayerId, PlayerPid);
        _ ->
          lager:info("--- player process is still alive, try bind ..."),
          set_agentpid(PlayerId, AgentPid) % 将新的Agent关联到已有Player进程上
      end
  end.

player_register(PlayerId, AgentPid, Param) ->
  player_sup:start_child(register, PlayerId, AgentPid, Param).


init_ets() ->
  ets:new(?MODULE, [public, set, named_table, {keypos, 2}]).

init_timer() ->
  erlang:send_after(?SAVE_TIME_SPAN, self(), save_time).

do_save() ->
  All = ets:tab2list(?MODULE),
  lists:foreach(fun(Item) ->
        player:save(Item#item.player_pid)
    end,
               All).

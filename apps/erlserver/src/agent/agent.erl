-module(agent).

-behaviour(ranch_protocol).
-behaviour(gen_server).

%% ranch_protocol callbacks
-export([start_link/4, init/4]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
%% API Functions
-export([send/2, kickout/1]).
%% ppid is the pid of player process
-record(state, {socket, transport, player_id}).

%%
%% the agent will be start by ranch when a new connection reach, by call start_link/4
%% then agent will recv socket data from this connection.
%%
start_link(Ref, Socket, Transport, ProtoOpts) ->
		proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, ProtoOpts]).

init(Ref, Socket, Transport, _ProtoOpts) ->
	lager:info("--- new agent.~n"),
	ok = proc_lib:init_ack({ok, self()}),
	ok = ranch:accept_ack(Ref),
	ok = Transport:setopts(Socket, [{active, once}]),
	State = #state{socket=Socket, transport=Transport, player_id = undefined},
	gen_server:enter_loop(?MODULE, [], State).

%% never used
init({}) ->
	{ok, undefined}.

handle_call(_Req, _From, State) ->
  {reply, ok, State}.

handle_cast({send, Data}, #state{socket=Socket, transport=Transport}=State) ->
  Transport:send(Socket, Data),
  {noreply, State};

handle_cast(kick, State) ->
  {stop, normal, State};

handle_cast(_Msg, State) ->
  {noreply, State}.

%% process socket data from ranch
%% agent decode socket data, create player process when login, then forward msg to player.
handle_info({tcp, Socket, Data}, #state{socket=Socket, transport=Transport, player_id=PlayerId}=State) ->
  % simple decode
  <<MsgId:8, Remaining/bytes>> = Data,
  case MsgId of
    0 -> 	
      {ok, NPlayerId} = login(Remaining),
	  Transport:setopts(Socket, [{active, once}]),
      lager:info("--- login ok with playerid: ~p", [NPlayerId]),
      {noreply, State#state{player_id=NPlayerId}};
	_ ->
      PlayerPid = player_manager:get_playerpid(PlayerId),
	  {Module, Fun} = msg_dispatcher:dispatch(MsgId),
	  gen_server:cast(PlayerPid, {packet, Module, Fun, Remaining}),
	  Transport:setopts(Socket, [{active, once}]),
	  {noreply, State}
	end;

handle_info({tcp_closed, _Socket}, State) ->
  {stop, normal, State};

handle_info({tcp_error, _, Reason}, State) ->
  {stop, Reason, State};

handle_info(timeout, State) ->
  {stop, normal, State};

handle_info(_Info, State) ->
  {stop, unknown_info, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, #state{player_id=PlayerId}) ->
  lager:info("--- close agent.~n"),
  player_manager:disconnect(PlayerId, self()),
  ok.
%% ==============================
%% API Functions
%% ==============================
send(AgendPid, Data) ->
	gen_server:cast(AgendPid, {send, Data}).

%% ==============================
%% Private Functions
%% ==============================
login(Data) ->
  lager:info("--- player login: ~p~n", [Data]),
  %TODO 验证登录逻辑 获取PlayerId
  <<PlayerId:8, _/bytes>> = Data,
  player_manager:connect(PlayerId, self()),
  player_manager:player_login(PlayerId, self(), no_param),
  {ok, PlayerId}.

kickout(AgentPid) ->
  gen_server:cast(AgentPid, kick).

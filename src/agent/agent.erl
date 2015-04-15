-module(agent).

-behaviour(ranch_protocol).
-behaviour(gen_server).

%% ranch_protocol callbacks
-export([start_link/4, init/4]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(state, {socket, transport}).

%%
%% the agent will be start by ranch when a new connection reach, by call start_link/4
%% then agent will recv socket data from this connection.
%%
start_link(Ref, Socket, Transport, ProtoOpts) ->
		proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, ProtoOpts]).

init(Ref, Socket, Transport, _ProtoOpts) ->
	io:format("---new agent.~n"),
	ok = proc_lib:init_ack({ok, self()}),
	ok = ranch:accept_ack(Ref),
	ok = Transport:setopts(Socket, [{active, once}]),
	State = #state{socket=Socket, transport=Transport},
	gen_server:enter_loop(?MODULE, [], State).

%% never used
init({}) ->
	{ok, undefined}.

handle_call(_Req, _From, State) ->
	{reply, ok, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

%% process socket data from ranch
handle_info({tcp, Socket, Data}, #state{socket=Socket, transport=Transport}=State) ->
	io:format("Recv data: ~p~n", [Data]),
	Transport:send(Socket, "WelCome To Server\n"),
	Transport:setopts(Socket, [{active, once}]),
	{noreply, State};

handle_info({tcp_closed, _Socket}, State) ->
	io:format("Recv tcp_closed ~n"),
	{stop, normal, State};

handle_info({tcp_error, _, Reason}, State) ->
	io:format("Recv tcp_error ~n"),
	{stop, Reason, State};

handle_info(timeout, State) ->
	{stop, normal, State};

handle_info(_Info, State) ->
	{stop, unknown_info, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(_Reason, _State) ->
	io:format("---close agent.~n"),
	ok.

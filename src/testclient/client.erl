-module(client).

-behaviour(gen_server).

%% Callbacks
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,code_change/3,terminate/2]).
%% API
-export([start/0, send/1, stop/0]).

-record(state, {socket}).

%% =========================
%% API
%% =========================
start() ->
	gen_server:start({local, ?MODULE}, ?MODULE, ["127.0.0.1", 6159], []).

stop() ->
	gen_server:cast(?MODULE, stop).

%% ===========================
%% Callbacks
%% ===========================
init([IP, Port]) ->
	{ok, Socket} = gen_tcp:connect(IP, Port, [binary, {active, true}, {packet, 0}]),
	{ok, #state{socket=Socket}}.

handle_call({send, Packet}, _From, State=#state{socket=Socket}) ->
	gen_tcp:send(Socket, Packet),
	{reply, ok, State};

handle_call(_Req, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

handle_cast(stop, State) ->
	{stop, normal, State};

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({tcp, _Socket, Data}, State) ->
	io:format("recv: ~p~n", [Data]),
	{noreply, State};

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State)->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ========================
%% Send API 
%% ========================
send(Packet) ->
	gen_server:call(?MODULE, {send, Packet}).

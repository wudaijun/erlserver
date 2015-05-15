-module(mongodb_server).

-behaviour(gen_server).

%% gen_server callbacks
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,code_change/3,terminate/2]).
%% API
-export([start_link/1, insert/2, find_one/2, update/3]).

-define(SERVER_NAME, ?MODULE).
-record(state, {conn}).

%% ================================
%% API
%% ================================

start_link(Database) ->
  gen_server:start_link({local, ?SERVER_NAME}, ?MODULE, [Database], []).

insert(Col, Data) ->
  gen_server:call(?SERVER_NAME, {insert, Col, Data}).

find_one(Col, Query) ->
  gen_server:call(?SERVER_NAME, {find_one, Col, Query}).

update(Col, Query, Data) ->
  gen_server:call(?SERVER_NAME, {update, Col, Query, Data}).

%% ===============================
%% Callbacks
%% ===============================

init([Database]) ->
  ok = application:start(bson),
  ok = application:start(crypto),
  ok = application:start(mongodb),
  {ok, Conn} = mongo:connect(Database),
  {ok, #state{conn=Conn}}.

handle_call({insert, Col, Data}, _From, State) ->
  Conn = State#state.conn,
  Res = mongo:insert(Conn, Col, Data),
  {reply, {ok, Res}, State};

handle_call({find_one, Col, Query}, _From, State) ->
  Conn = State#state.conn,
  Res = mongo:find_one(Conn, Col, Query),
  {reply, {ok, Res}, State};

handle_call({update, Col, Query, Data}, _From, State)->
  Conn = State#state.conn,
  lager:info("update arg: Conn:~p, Col:~p, Query:~p, Data:~p~n", [Conn, Col, Query, Data]),
  Res = mongo:update(Conn, Col, Query, Data),
  lager:info("update res: ~p~n", [Res]),
  {reply, {ok, Res}, State};

handle_call(_Req, _From, State) ->
  {noreply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, _State) ->
  application:stop(mongodb),
  application:stop(bson),
  ok.


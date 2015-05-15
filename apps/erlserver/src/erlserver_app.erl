-module(erlserver_app).

-behaviour(application).

%% Callbacks
-export([start/2, stop/1]).

%% API
-export([start/0]).

start(_StartType, _StartArgs) ->
  ok = lager:start(),
  case erlserver_sup:start_link() of
  	{ok, Pid} -> {ok, Pid};
  	Other -> {error, Other}
  end.

stop(_State) ->
	ok.

% ========================
% API
% ========================
start() ->
  ok = application:start(erlserver).

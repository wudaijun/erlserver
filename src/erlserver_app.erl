-module(erlserver_app).

-behaviour(application).

%% Callbacks
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  application:start(lager),
  case erlserver_sup:start_link() of
  	{ok, Pid} -> {ok, Pid};
  	Other -> {error, Other}
  end.

stop(_State) ->
	ok.

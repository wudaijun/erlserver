-module(module2).

-export([cb2/1]).

cb2(Data) ->
	io:format("Call cb2 with data: ~p~n", [Data]).

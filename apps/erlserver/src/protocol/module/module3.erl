-module(module3).

-export([cb3/1]).

cb3(Data) ->
	io:format("Call cb3 with data: ~p~n", [Data]).

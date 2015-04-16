-module(module1).

-export([cb1/1]).

cb1(Data) ->
	io:format("Call cb1 with data: ~p~n", [Data]).

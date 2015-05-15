-module(module2).

-export([get_name/1]).

get_name(Data) ->
	io:format("Call get_name with data: ~p~n", [Data]),
	{ok, {Res}} = model:find_one(<<"person">>, {name, Data}),
	io:format("res from db: ~p~n", [Res]),
	{Id} = bson:at('_id', Res),
	Id.

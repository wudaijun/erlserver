-module(module1).

-export([save_name/1]).

%% save data to db
save_name(Data) ->
	io:format("Call save_name with data: ~p~n", [Data]),
	{ok, [Res]} = model:insert(<<"person">>, [{name, Data}]),
	io:format("res from db: ~p~n", [Res]),
	{Id} = bson:at('_id', Res),
	Id.

-module(model).

-export([insert/2, 
         find_one/2, 
         update/3, 
         transaction/1, 
         get_bson_value/2 
        ]).

%% ================================================================
%% this is a simple wrapper of mongodb_server
%% by using this helper module. you can just use mongodb like a rpc
%% ================================================================

-callback(collection() -> atom()).
-callback(init(term()) -> term()).

%% return _id of new document
insert(Col, Data) ->
  mongodb_server:insert(Col, Data).

%% return the document mongodb return   
find_one(Col, Query) ->
  mongodb_server:find_one(Col, Query).

update(Col, Query, Data)  ->
  mongodb_server:update(Col, Query, Data).

get_bson_value(Key, Bson) ->
  case bson:lookup(Key, Bson) of
    {Value} -> {ok, Value};
    {} -> {error, not_exist}
  end.

transaction(Fun) ->
  Res = Fun(),
  {ok, Res}.

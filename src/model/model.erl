-module(model).

-export([insert/2, find_one/2]).

%% ================================================================
%% this is a simple wrapper of mongodb_server
%% by using this helper module. you can just use mongodb like a rpc
%% ================================================================

%% return _id of new document
insert(Collection, Data) ->
	mongodb_server:insert(Collection, Data).

%% return the document mongodb return   
find_one(Collection, Data) ->
	mongodb_server:find_one(Collection, Data).

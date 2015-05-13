%%% ================================================
%%% @author wudaijun 05-11
%%% @doc 统一管理进程字典 标记进程字典中的数据变化
%%%			 主要提供接口:
%%%			 		1.进程字典的初始化接口
%%%			 		2.进程字典的读写接口
%%%			 		3.获取进程字典的变更数据
%%%			 		4.重置进程字典的变更状态
%%% 
%%% 		实现原理:
%%% 				进程字典分为single 和 list 两种类型。而state主要是对list类型进行单个条目的状态跟踪:
%%%
%%% 				------ list Model ----------
%%% 				存放list中各个key的状态
%%% 				{Name, list} -> [{key1, update}, {key2, delete}, ...]
%%%
%%% 				存放列表中各元素的实际数据
%%% 				{Name, Key} ->  Data
%%%
%%% 				存放被删除的元素列表(将不能通过{Name, Key}找到)
%%% 				{Name, delete} -> [DeleteData1, DeleteData2, ....]
%%%
%%% 				------- single Model -------
%%%
%%% 				通过 Name 存取
%%% 				Name -> Value
%%%
%%% 				存放Model的更改状态
%%% 				{Name, state} -> State
%%% @end
%%% ================================================

-module(state).

% list model 存取
-export([ insert/3, insert/4, update/3, delete/2]).

% single model 存取
-export([ new/2, new/3, update/2 ]).

% 获取变更数据
-export([
  get_list_change/1,
  get_single_change/1,
  reset_list_change/1,
  reset_single_change/1
  ]).

% 辅助函数
-export([ status/ 1]).

%% 条目状态
-define(STATE_ORIGIN, state_origin).  % 与数据库处于同步状态
-define(STATE_NEW,    state_new).     % 数据库还没有该数据
-define(STATE_UPDATE, state_update).  % 数据库需要更新该数据
-define(STATE_DELETE, state_delete).  % 数据库需要删除该数据

%% 辅助key
-define(KEY_LIST, list).    % for list model
-define(KEY_DELETE, delete).% for list model
-define(KEY_STATE, state).  % for single model

%% ==================================================
%% 进程字典存取
%% ==================================================

% --------------------------------------------------
% list model 存取接口
% --------------------------------------------------

% 查询数据 --list model
-spec query(atom(), term()) -> term().
query(Name, Key) ->
  get({Name, Key}).

% 插入一条新数据 需要写入数据库 --list model
% 如果数据已存在，则只做插入，不跟踪状态
-spec insert(atom(), term(), term()) -> ok.
insert(Name, Key, Value) ->
  % 追踪状态
  case get({Name, ?KEY_LIST}) of
    undefined -> put({Name, ?KEY_LIST}, [{Key, ?STATE_NEW}]);
    KeyStateList ->
      case lists:keyfind(Key, 1, KeyStateList) of
        false ->
          put({Name, ?KEY_LIST}, [{Key, ?STATE_NEW}|KeyStateList]);
        _ ->
          lager:info("record {~p ~p} already exist~n", [Name, Key]),
          undefined % 新添加数据已存在
      end
  end,
  % 实际写入
  put({Name, Key}, Value),
  ok.

% 插入一条初始化数据 无需写入数据库 --list model
% 如果数据已存在，则只做插入，不跟踪状态
-spec insert(atom(), term(), term(), init) -> ok.
insert(Name, Key, Value, init) ->
  % 追踪状态
  case get({Name, ?KEY_LIST}) of
    undefined -> put({Name, ?KEY_LIST}, [{Key, ?STATE_ORIGIN}]);
    KeyStateList ->
      case lists:keyfind(Key, 1, KeyStateList) of
        false ->
          put({Name, ?KEY_LIST}, [{Key, ?STATE_ORIGIN}|KeyStateList]);
        _ ->
          lager:info("record {~p ~p} already exist~n", [Name, Key]),
          undefined
      end
  end,
  % 实际写入
  put({Name, Key}, Value),
  ok.

% 更新数据 --list model
% 如果该Key不存在,则不作操作
-spec update(atom(), term(), term()) -> ok.
update(Name, Key, Value) ->
  case get({Name, ?KEY_LIST}) of
    undefined -> ok;
    KeyStateList ->
      case lists:keyfind(Key, 1, KeyStateList) of
        false -> ok;
        {Key,State}=OldState ->
          case State of             % 跟踪状态
            ?STATE_ORIGIN->
              put({Name, Key}, Value),
              NewList = lists:delete(OldState, KeyStateList),
              put({Name, ?KEY_LIST}, [{Key, ?STATE_UPDATE}|NewList]),
              ok;
            ?STATE_DELETE->
              lager:info("update a deleted data: {~p ~p}", [Name, Key]);
            _ -> 
              put({Name, Key}, Value),  % 更新数据
              ok   % UPDATE 和 NEW 状态不变
          end
      end
  end.

% 删除数据 --list model
-spec delete(atom(), term()) -> ok.
delete(Name, Key) ->
  case get({Name, ?KEY_LIST}) of
    undefined -> ok;
    KeyStateList ->
      case lists:keyfind(Key, 1, KeyStateList) of
        false -> ok;
        {Key, State}=OldState ->
          case State of
            ?STATE_ORIGIN ->
              NewList = lists:delete(OldState, KeyStateList),
              put({Name, ?KEY_LIST}, [{Key, ?STATE_DELETE}|NewList]),
              add_to_delete_list(Name, query(Name, Key));
            ?STATE_UPDATE ->
              NewList = lists:delete(OldState, KeyStateList),
              put({Name, ?KEY_LIST}, [{Key, ?STATE_DELETE}|NewList]),
              add_to_delete_list(Name, query(Name, Key));
            ?STATE_DELETE ->
              lager:info("delete {~p ~p} twice~n", [Name, Key]);
            ?STATE_NEW -> % NEW状态还未存入数据库  直接删除状态记录和数据
              NewList = lists:delete(OldState, KeyStateList),
              put({Name, ?KEY_LIST}, NewList)
          end
      end
  end,
  erase({Name, Key}),
  ok.

add_to_delete_list(Name, Value) ->
  case get({Name, ?KEY_DELETE}) of
    undefined ->
      put({Name, ?KEY_DELETE}, [Value]);
    DelList ->
      put({Name, ?KEY_DELETE}, [Value|DelList])
  end.


% ---------------------------------------------------
% single model 存取接口 
% single model 没有删除接口和删除状态
% --------------------------------------------------

-spec new(atom(), term()) -> ok.
new(Name, Value) ->
  put({Name, ?KEY_STATE}, ?STATE_NEW),
  put(Name, Value),
  ok.

-spec new(atom(), term(), init) -> ok.
new(Name, Value, init) ->
  put({Name, ?KEY_STATE}, ?STATE_ORIGIN),
  put(Name, Value),
  ok.

% 如果数据不存在，不会新建
-spec update(atom(), term()) -> ok|undefined.
update(Name, Value) ->
  case get({Name, ?KEY_STATE}) of
    undefined -> undefined;
    State ->
      put(Name, Value),
      case State of
        ?STATE_ORIGIN ->
          put({Name, ?KEY_STATE}, ?STATE_UPDATE),
          ok;
        _ -> ok
      end
  end.

%% ==================================================
%% 获取进程字典数据变更
%% ==================================================

% 获取list model的变更数据 
% 返回列表三元组 {NewList, UpdateList, DeleteList}
-spec get_list_change(atom()) -> {list(), list(), list()}.
get_list_change(Name) ->
  KeyStateList = get({Name, ?KEY_LIST}),
  {NList, UList} = lists:foldl(fun({Key, State}, {NewList, UpdateList}) ->
        case State of
          ?STATE_NEW ->
            NewNewList = [query(Name, Key)|NewList],
            {NewNewList, UpdateList};
          ?STATE_UPDATE ->
            NewUpdateList = [query(Name, Key)|UpdateList],
            {NewList, NewUpdateList};
          _ ->
            {NewList, UpdateList}
        end
    end,
    {[],[]},
    KeyStateList),
  case get({Name, ?KEY_DELETE}) of
    undefined -> {NList, UList, []};
    DList -> {NList, UList, DList}
  end.

% 获取single model的变更数据
% 返回{State, Value}  State: new update other
-spec get_single_change(atom()) -> {atom(), term()}.
get_single_change(Name) ->
  State = case get({Name, ?KEY_STATE}) of
    ?STATE_NEW -> new;
    ?STATE_UPDATE -> update;
    _ -> other
  end,
  {State, get(Name)}.

% 重置 list model 状态
% 调用该接口前，确认所有变更数据均已落地
-spec reset_list_change(atom()) -> ok.
reset_list_change(Name) ->
  case get({Name, ?KEY_LIST}) of
    undefined -> ok;
    KeyStateList ->
      List = lists:foldl(fun({Key, State}, NewKeyStateList) ->
            case State of
              ?STATE_DELETE ->
                erase({Name, Key}),
                NewKeyStateList;
              _ -> 
                [{Key, ?STATE_ORIGIN}|NewKeyStateList]
            end
        end,
        [],
        KeyStateList),
        put({Name, ?KEY_LIST}, List) % 重置所有条目状态为ORIGIN
  end,
  erase({Name, ?KEY_DELETE}), % 删除被标记为删除的数据
  ok.

% 重置 single model 状态
-spec reset_single_change(atom()) -> ok.
reset_single_change(Name) ->
  put({Name, ?KEY_STATE}, ?STATE_ORIGIN).

% === 测试使用 输出进程字典各项统计信息
status(Name) ->
  lager:info("~n--- Key States ---~n"),
  lager:info("~p", [get({Name, ?KEY_LIST})]),
  lager:info("~n--- Delete Key ---~n"),
  lager:info("~p~n", [get({Name, ?KEY_DELETE})]).

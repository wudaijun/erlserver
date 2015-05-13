-module(model_task).
-behaviour(model).
-compile([export_all]).

%% =============================
%% model Callbacks
%% =============================

collection() -> ?MODULE.

% 从数据库加载数据到进程字典中
init(PlayerId) ->
  {ok, Res} = model:find_one(collection(), {'_id', PlayerId}),
  io:format("model_task init: ~p~n", [Res]),
  case Res of
    {} -> % 初始化
      model:insert(collection(), {'_id', PlayerId, tasks, []});
    {Doc} ->
      {ok, Tasks} = model:get_bson_value(tasks, Doc),
      lists:foreach(fun(Task) ->
        {ok, TaskId} = model:get_bson_value(task_id, Task),
        ok = state:insert(collection(), TaskId, Task, init)
      end,
    Tasks)
  end,
  Res.

% 进程字典中的数据落地
list_change(PlayerId, Data) ->
  io:format("model_task list_change: ~p~n", [Data]),
  {NewData, UpData, DelData} = Data,
  % 处理新增数据
  lists:foreach(fun(NewTask) -> 
        model:update(collection(), {'_id', PlayerId}, {'$push', {tasks, NewTask}})
    end, NewData),
  % 处理更新数据
  lists:foreach(fun(UpTask) -> 
        {ok, TaskId} = model:get_bson_value(task_id, UpTask),
        model:update(collection(), {'_id', PlayerId, 'tasks.task_id', TaskId}, {'$set', {'tasks.$', UpTask}})
    end, UpData), 
  % 处理删除数据
  lists:foreach(fun(DelTask) ->
        {ok, TaskId} = model:get_bson_value(task_id, DelTask),
        model:update(collection(), {'_id', PlayerId}, {'$pull', {tasks, {'task_id', TaskId}}}) 
    end, DelData).

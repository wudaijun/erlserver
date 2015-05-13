%%% ==========================================
%%% @author wudaijun 5.12
%%% @doc 简单的任务控制器 
%%% @end
%%% ==========================================

-module(player_task).

-compile([export_all]).


init_callback(Doc) ->
  io:format("player_task:init_callback: ~p~n", [Doc]),
  ok.

% -- 测试接口
test() ->
  io:format("start mongodb ....~n"),
  mongodb_server:start_link(<<"sampledb">>),

  io:format("init data .....~n"),
  player_model:init(123),

  %io:format("insert some data to state ....~n"),
  %state:insert(model_task, 101, {task_id, 101, task_trace, 3, task_state, 2}),
  %state:insert(model_task, 102, {task_id, 102, task_trace, 0, task_state, 1}),
  
  %io:format("delete data ....~n"),
  %state:delete(model_task, 101),

  io:format("modify data ...~n"),
  state:update(model_task, 101, {task_id, 101, task_trace, 998, task_state, 3}),

  io:format("try to saving ....~n"),
  player_model:save(123).

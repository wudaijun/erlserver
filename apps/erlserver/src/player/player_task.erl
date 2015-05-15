%%% ==========================================
%%% @author wudaijun 5.12
%%% @doc 简单的任务控制器 
%%% @end
%%% ==========================================

-module(player_task).

-compile([export_all]).


init_callback(Doc) ->
  lager:info("player_task:init_callback: ~p~n", [Doc]),
  ok.

% -- 测试接口
test() ->
  lager:info("start mongodb ....~n"),
  mongodb_server:start_link(<<"sampledb">>),

  lager:info("init data .....~n"),
  player_model:init(123),

  %lager:info("insert some data to state ....~n"),
  %state:insert(model_task, 101, {task_id, 101, task_trace, 3, task_state, 2}),
  %state:insert(model_task, 102, {task_id, 102, task_trace, 0, task_state, 1}),
  
  %lager:info("delete data ....~n"),
  %state:delete(model_task, 101),

  lager:info("modify data ...~n"),
  state:update(model_task, 101, {task_id, 101, task_trace, 998, task_state, 3}),

  lager:info("try to saving ....~n"),
  player_model:save(123).

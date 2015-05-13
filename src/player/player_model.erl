%%% ================================================ 
%%% @author wudaijun 5.12
%%% @doc
%%%   该模块负责建立controller到model之间的映射,
%%%   并且管理所有model的加载和落地
%%%   
%%%   1. 加载 player_model:init(PlayerId)
%%%     加载完成之后回调对应controller:init_callback
%%%   
%%%   2. 落地 player_model:save(PlayerId)
%%% @end
%%% =============================================== 

-module(player_model).

-compile([export_all]).

% 所有model和对应controller信息
cm_map() ->
  % 格式 {controller_module, {model_name, model_type}, model_module}
  [
    {player_task, {model_task:collection(), list}, model_task}
  ].

% 加载所有model
init(PlayerId) ->
  case  model:transaction(fun()-> 
          [{Controller, Model:init(PlayerId)} || {Controller, _, Model} <- cm_map()] 
      end) of
    {ok, InitDataList} -> 
      lists:foreach(fun({Controller, InitData}) -> Controller:init_callback(InitData) end, InitDataList),
      ok
  end.

% 保存所有的model
save(PlayerId) ->
  % 获取变更数据
  UpdatedData = [{Model, Type, get_data(Name, Type)} || {_, {Name, Type}, Model} <- cm_map()],
  % 保存数据
  case model:transaction(fun()-> save_data(PlayerId, UpdatedData) end) of
    {ok, _Result} ->
          lists:foreach(fun reset_data/1, cm_map()) % 重置数据状态
  end,
  ok.


% 获取model更新数据
get_data(Name, Type) ->
  case Type of
    single ->
      state:get_single_change(Name);
    list ->
      state:get_list_change(Name)
  end.

% 保存所有model更新数据
save_data(PlayerId, UpdatedData) ->
  lists:foreach(fun({Model, Type, Data})->
          case Type of
            single ->
              Model:single_change(PlayerId, Data);
            list ->
              Model:list_change(PlayerId, Data)
          end
    end, UpdatedData).

reset_data({_, {Name, Type}, _}) ->
  case Type of
    single ->
      state:reset_single_change(Name);
    list ->
      state:reset_list_change(Name)
  end.

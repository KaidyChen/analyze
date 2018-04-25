-module(analyze_lighting_strategy_store).

-include("analyze_store.hrl").

-export([
         load/0,
         close/0,
         get_timer_strategy_list/0,
         get_data_list_by_task_id/1,
         get_strategy_list/0,
         insert/1,
         delete/1,
         lookup/1
        ]).

-define(DISK_TAB_ID, strategy_task_disk).
-define(DISK_TAB_FILE, "./data/lighting_strategy_task.data").

load() ->
    create_table(),
    restore_backup(),
    ok.

create_table() ->
    dets:open_file(?DISK_TAB_ID, [{file, ?DISK_TAB_FILE}, {keypos, #lighting_strategy_rd.task_id}, 
                                  {access, read_write}, {type, set}]),
    ok.

restore_backup() ->
    Fun =
        fun(Lighting_strategy_rd) ->
                erlang:put(Lighting_strategy_rd#lighting_strategy_rd.task_id, Lighting_strategy_rd),
                continue
        end,
    dets:traverse(?DISK_TAB_ID, Fun),
    ok.
    
close() ->
    dets:close(?DISK_TAB_ID),
    ok.
    
insert(Lighting_strategy_rd) ->
    erlang:put(Lighting_strategy_rd#lighting_strategy_rd.task_id, Lighting_strategy_rd),
    dets:insert(?DISK_TAB_ID, Lighting_strategy_rd).

lookup(Task_id) ->
    case erlang:get(Task_id) of
        undefined ->
            {error, not_found};
        Value when is_record(Value, lighting_strategy_rd) ->
            {ok, Value};
        _ ->
            {error, not_found}
    end.

delete(Task_id) ->
    erlang:erase(Task_id),
    dets:delete(?DISK_TAB_ID, Task_id).

get_timer_strategy_list() ->
    Pred =
        fun(Key) ->
                case erlang:get(Key) of
                    Value when is_record(Value, lighting_strategy_rd)
                               andalso (Value#lighting_strategy_rd.strategy_type =:= ?TIMER_TYPE) ->
                        {true, Value};
                    _ ->
                        false
                end
        end,
    lists:filtermap(Pred, erlang:get_keys()).

get_strategy_list() -> 
    Pred =
        fun(Key) ->
                case erlang:get(Key) of
                    Value when is_record(Value, lighting_strategy_rd) ->    
                        {true, Value};
                    _ ->
                        false
                end
        end,
    lists:filtermap(Pred, erlang:get_keys()).
   

get_data_list_by_task_id(TaskId) ->
    case lookup(TaskId) of
        {ok, Lighting_strategy_rd} ->
            {ok, Lighting_strategy_rd#lighting_strategy_rd.data_list};
        _ ->
            {error, not_found}
    end.
                                                                      

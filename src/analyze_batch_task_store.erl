-module(analyze_batch_task_store).

-include("batch_task.hrl").

-export([load_task/0,
         insert_task/1,
         delete_task/1,
         lookup_task/1,
         get_task_list/0,
         get_batch_task_by_condition/1
]).

-define(BATCH_TASK_DATA, "./data/batch_task.data").
-define(BATCH_TASK_RAM_TABLE, batch_task_ram).
-define(BATCH_TASK_DISK_TABLE, batch_task_disk).

-define(BATCH_TASK_RESULT_DATA, "./data/batch_task_result.data").
-define(BATCH_TASK_RESULT_RAM_TABLE, batch_task_result_ram).
-define(BATCH_TASK_RESULT_DISK_TABLE, batch_task_result_disk).

load_task() ->
    create_table(),
    restore_backup().

create_table() ->
    Keypos = #batch_task.task_id,
    ets:new(?BATCH_TASK_RAM_TABLE, [named_table, set, protected, {keypos, Keypos}, {read_concurrency, true}]),
    dets:open_file(?BATCH_TASK_DISK_TABLE, [{file, ?BATCH_TASK_DATA}, {type, set}, {keypos, Keypos}, {access, read_write}]),

    ets:new(?BATCH_TASK_RESULT_RAM_TABLE, [named_table, set, protected]),
    dets:open_file(?BATCH_TASK_RESULT_DISK_TABLE, [{file, ?BATCH_TASK_RESULT_DATA}, {access, read_write}, 
        {type, set}]),
    ok.

restore_backup() ->
    Insert_1 = fun(Task) ->
            ets:insert(?BATCH_TASK_RAM_TABLE, Task),
            continue
    end,
    dets:traverse(?BATCH_TASK_DISK_TABLE, Insert_1),

    Insert_2 = fun(Task_result) ->
            ets:insert(?BATCH_TASK_RESULT_RAM_TABLE, Task_result),
            continue
    end,
    dets:traverse(?BATCH_TASK_RESULT_DISK_TABLE, Insert_2),
    ok.

close_table() ->
    ets:delete(?BATCH_TASK_RAM_TABLE),
    dets:close(?BATCH_TASK_DISK_TABLE),

    ets:delete(?BATCH_TASK_RESULT_RAM_TABLE),
    dets:close(?BATCH_TASK_RESULT_DISK_TABLE),
    ok.

insert_task(Task_or_tasks) ->
    ets:insert(?BATCH_TASK_RAM_TABLE, Task_or_tasks),
    case dets:insert(?BATCH_TASK_DISK_TABLE, Task_or_tasks) of
        ok              -> ok;
        {error, Reason} -> {error, Reason}
    end.

lookup_task(Task_id) ->
    case ets:lookup(?BATCH_TASK_RAM_TABLE, Task_id) of
        [Task | _] ->
            {ok, Task};
        [] ->
            {error, not_found}
    end.

delete_task(Task_id) ->
    Key = Task_id,
    ets:delete(?BATCH_TASK_RAM_TABLE, Key),
    case dets:delete(?BATCH_TASK_DISK_TABLE, Key) of
        ok              -> ok;
        {error, Reason} -> {error, Reason}
    end.

get_task_list() ->
    ets:tab2list(?BATCH_TASK_RAM_TABLE).

get_batch_task_by_condition(Condition) ->
    Batch_task = #batch_task{
        task_id = '_',
        task_status = '_',
        datetime_start = '_',
        datetime_end = '_',
        operation_type = '_',
        operation_argv = '_',
        condition = Condition
    },
    case ets:match_object(?BATCH_TASK_RAM_TABLE, Batch_task) of
        [] -> {error, not_found};
        Batch_task_list -> {ok, Batch_task_list}       
    end.




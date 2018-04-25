-module(analyze_strategy_task_store).

-export([
         load/0,
         close/0,
         insert/2,
         delete/1,
         lookup/1
        ]).

-define(RAM_TAB_ID, strategy_task_ram).
-define(DISK_TAB_ID, strategy_task_disk).
-define(DISK_TAB_FILE, "./data/strategy_task").

load() ->
    create_table(),
    restore_backup(),
    ok.

create_table() ->
    ets:new(?RAM_TAB_ID, [set, named_table, protected]),
    dets:open_file(?DISK_TAB_ID, [{file, ?DISK_TAB_FILE}, {access, read_write}, {type, set}]),
    ok.

restore_backup() ->
    Fun =
        fun(Obj) ->
                ets:insert(?RAM_TAB_ID, Obj),
                continue
        end,
    dets:traverse(?DISK_TAB_ID, Fun),
    ok.
    
close() ->
    ets:delete(?RAM_TAB_ID),
    dets:close(?DISK_TAB_ID),
    ok.
    
insert(Task_id, Gateway_cmd_list) ->
    Object = {Task_id, Gateway_cmd_list},
    ets:insert(?RAM_TAB_ID, Object),
    dets:insert(?DISK_TAB_ID, Object).

lookup(Task_id) ->
    case ets:lookup(?RAM_TAB_ID, Task_id) of
        [] ->
            {error, not_found};
        [{Task_id, Gateway_cmd_list}] ->
            {ok, Gateway_cmd_list}
    end.

delete(Task_id) ->
    ets:delete(?RAM_TAB_ID, Task_id),
    dets:delete(?DISK_TAB_ID, Task_id).

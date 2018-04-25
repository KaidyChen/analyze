-module(analyze_realtime_workflow).

-export([load_workflow/0, insert/3, lookup/2]).

-define(WORKFLOW_DATAFILE, "./data/realtime_workflow.data").
-define(RAM_TABLE_ID, realtime_workflowRam).
-define(DISK_TABLE_ID, realtime_workflowDisk).

load_workflow() ->
    create_table(),
    restore_backup().

create_table() ->
    ets:new(?RAM_TABLE_ID, [named_table, set, public, {write_concurrency, true}]),
    filelib:ensure_dir(?WORKFLOW_DATAFILE),
    dets:open_file(?DISK_TABLE_ID, [{file, ?WORKFLOW_DATAFILE}, {access, read_write}, {type, set}]).

restore_backup() ->
    Insert = fun(Obj) ->
            ets:insert(?RAM_TABLE_ID, Obj),
            continue
    end,
    dets:traverse(?DISK_TABLE_ID, Insert).

insert(Meter_type, Meter, Realtime_workflow) ->
    Obj = {{Meter_type, Meter}, Realtime_workflow},
    ets:insert(?RAM_TABLE_ID, Obj),
    case dets:insert(?DISK_TABLE_ID, Obj) of
        ok              -> ok;
        {error, Reason} -> {error, Reason}
    end.

lookup(Meter_type, Meter) ->
    case ets:lookup(?RAM_TABLE_ID, {Meter_type, Meter}) of
        [] ->        {error, not_found};
        [{{Meter_type, Meter}, Realtime_workflow} | _] -> {ok, Realtime_workflow}
    end.


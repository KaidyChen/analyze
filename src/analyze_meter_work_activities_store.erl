-module (analyze_meter_work_activities_store).

-export([load_activities/0, insert/3, lookup/2]).

-define(ACTIVITIES_DATA, "./data/work_activities.data").
-define(RAM_TABLE_ID, work_activitiesRam).
-define(DISK_TABLE_ID, work_activitiesDisk).

load_activities() ->
    create_table(),
    restore_backup(),
    ok.

create_table() ->
    ets:new(?RAM_TABLE_ID, [named_table, set, public, {write_concurrency, true}]),
    dets:open_file(?DISK_TABLE_ID, [{file, ?ACTIVITIES_DATA}, {access, read_write}, {type, set}]).

restore_backup() ->
    Insert = fun(Obj) ->
            ets:insert(?RAM_TABLE_ID, Obj),
            continue
    end,
    dets:traverse(?DISK_TABLE_ID, Insert).

insert(Meter_type, Meter, Work_activities) ->
    Obj = {{Meter_type, Meter}, Work_activities},
    ets:insert(?RAM_TABLE_ID, Obj),
    case dets:insert(?DISK_TABLE_ID, Obj) of
        ok              -> ok;
        {error, Reason} -> {error, Reason}
    end.

lookup(Meter_type, Meter) ->
    case ets:lookup(?RAM_TABLE_ID, {Meter_type, Meter}) of
        [] ->        {error, not_found};
        [{{Meter_type, Meter}, Work_activities} | _] -> {ok, Work_activities}
    end.

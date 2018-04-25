-module (analyze_build_time_table_store).

-include("time_table.hrl").
-include("print.hrl").

-export([load_time_table/0,
         insert/1,
         lookup_by_room_id/1,
         lookup_by_room_id_and_level/2

]).

-define(BUILD_TIME_TABLE_DATA, "./data/build_time_table.data").
-define(BUILD_TIME_TABLE_RAM_TABLE, build_time_table_ram).
-define(BUILD_TIME_TABLE_DISK_TABLE, build_time_table_disk).

load_time_table() ->
    create_table(),
    restore_backup(),
    ok.

create_table() ->
    Keypos = #time_table.key,
    ets:new(?BUILD_TIME_TABLE_RAM_TABLE, [named_table, set, public, {keypos, Keypos}, {read_concurrency, true}]),
    dets:open_file(?BUILD_TIME_TABLE_DISK_TABLE, [{file, ?BUILD_TIME_TABLE_DATA}, {keypos, Keypos}, {type,set}, {access, read_write}]),
    ok.

restore_backup() ->
    Insert = fun
        (Time_table) ->
            ets:insert(?BUILD_TIME_TABLE_RAM_TABLE, Time_table),
            continue

    end,
    dets:traverse(?BUILD_TIME_TABLE_DISK_TABLE, Insert),
    ok.

insert(Time_table_or_list) ->
    ets:insert(?BUILD_TIME_TABLE_RAM_TABLE, Time_table_or_list),
    case dets:insert(?BUILD_TIME_TABLE_DISK_TABLE, Time_table_or_list) of
        ok -> ok;
        {error, Reason} -> {error, Reason}
    end.

lookup_by_room_id_and_level(Room_id, Level) ->
    Pattern = #time_table{
            key = '_',
            room_id = Room_id,
            type_and_labels_of_meter = '_',
            level = Level,
            validity_date_start = '_',
            validity_date_end = '_',
            holiday_mode = '_',
            time_list = '_'
    },
    case ets:match_object(?BUILD_TIME_TABLE_RAM_TABLE, Pattern) of
        [] -> {error, not_found};
        Time_table_list -> {ok, Time_table_list}
    end.

lookup_by_room_id(Room_id) ->
    Pattern = #time_table{
            key = '_',
            room_id = Room_id,
            type_and_labels_of_meter = '_',
            level = '_',
            validity_date_start = '_',
            validity_date_end = '_',
            holiday_mode = '_',
            time_list = '_'
    },
    case ets:match_object(?BUILD_TIME_TABLE_RAM_TABLE, Pattern) of
        [] -> {error, not_found};
        Time_table_list -> {ok, Time_table_list}
    end.











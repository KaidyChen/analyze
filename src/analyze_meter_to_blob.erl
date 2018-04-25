-module(analyze_meter_to_blob).

-export([load_blob/0,
         close_table/0,
         insert/2,
         lookup/1,
         to_list/0
]).

-define(METER_BLOB_DATA, "data/meter_blob.data").
-define(RAM_TABLE_ID, meter_blob_ram).
-define(DISK_TABLE_ID, meter_blob_disk).

load_blob() ->
    create_table(),
    restore_backup(),
    ok.

create_table() ->
    ets:new(?RAM_TABLE_ID, [set, public, named_table, {write_concurrency, true}]),
    dets:open_file(?DISK_TABLE_ID, [{file, ?METER_BLOB_DATA}, {access, read_write}, {type, set}]),
    ok.

restore_backup() ->
    Insert = fun
        (Obj) ->
            ets:insert(?RAM_TABLE_ID, Obj),
            continue
    end,
    dets:traverse(?DISK_TABLE_ID, Insert),
    ok.

close_table() ->
    ets:delete(?RAM_TABLE_ID),
    dets:close(?DISK_TABLE_ID),
    ok.

insert({_Meter_Type, _Meter_Str} = Key, Meter_blob) ->
    Obj = {Key, Meter_blob},
    ets:insert(?RAM_TABLE_ID, Obj),
    case dets:insert(?DISK_TABLE_ID, Obj) of
        ok -> ok;
        {error, Reason} -> {error, Reason}
    end.

lookup({_Meter_Type, _Meter_Str} = Key) ->
    case ets:lookup(?RAM_TABLE_ID, Key) of
        [{Key, Meter_blob}] -> 
            {ok, Meter_blob};
        [] -> 
            {error, not_found}
    end.
 
to_list() ->
    ets:tab2list(?RAM_TABLE_ID).



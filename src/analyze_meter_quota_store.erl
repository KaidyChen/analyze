-module (analyze_meter_quota_store).

-include("meter_quota.hrl").

-export([load/0,
         close_table/0,
         insert/1,
         lookup/1,
         lookup/2,
         delete/1,
         delete/2
]).

-define(METER_QUOTA_DATA, "./data/meter_quota.data").
-define(METER_QUOTA_RAM, meter_quota_ram).
-define(METER_QUOTA_DISK, meter_quota_disk).

load() ->
    create_table(),
    restore_backup(),
    ok.

create_table() ->
    Keypos = #meter_quota.key,
    ets:new(?METER_QUOTA_RAM, [named_table, set, protected, {keypos, Keypos}, {read_concurrency, true}]),
    dets:open_file(?METER_QUOTA_DISK, [{file, ?METER_QUOTA_DATA}, {keypos, Keypos}, {type, set}, {access, read_write}]),
    ok.

close_table() ->
    ets:delete(?METER_QUOTA_RAM),
    dets:close(?METER_QUOTA_DISK),
    ok.

restore_backup() ->
    Insert = fun
        (Meter_quota) ->
            ets:insert(?METER_QUOTA_RAM, Meter_quota),
            continue

    end,
    dets:traverse(?METER_QUOTA_DISK, Insert),
    ok.

insert(Meter_quota) ->
    ets:insert(?METER_QUOTA_RAM, Meter_quota),
    case dets:insert(?METER_QUOTA_DISK, Meter_quota) of
        ok -> ok;
        {error, Reason} -> {error, Reason}
    end.

lookup(Meter_type, Meter) ->
    lookup({Meter_type, Meter}).

lookup({Meter_type, Meter} = Key) ->
    case ets:lookup(?METER_QUOTA_RAM, Key) of
        [] -> {error, not_found};
        [Meter_quota | _] -> {ok, Meter_quota}
    end.

delete(Meter_type, Meter) ->
    delete({Meter_type, Meter}).

delete({Meter_type, Meter} = Key) ->
    ets:delete(?METER_QUOTA_RAM, Key),
    case dets:delete(?METER_QUOTA_DISK, Key) of
        ok -> ok;
        {error, Reason} -> {error, Reason}
    end.


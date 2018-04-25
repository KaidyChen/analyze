-module(analyze_meter_pid).

-export([init/0,
         insert/3,
         insert/2,
         lookup/2,
         delete/1,
         get_running_pid/2,
         lookup_by_pid/1,
         select_meter_by_meter_type/1,
         get_all_meter_type_and_meter/0
]).

-define(TABLE_ID, ?MODULE).

init() ->
    ets:new(?TABLE_ID, [set, public, named_table]).

insert(Meter_type, Meter, Pid) ->
    ets:insert(?TABLE_ID, {{Meter_type, Meter}, Pid}).

insert({Meter_type, Meter}, Pid) ->
    ets:insert(?TABLE_ID, {{Meter_type, Meter}, Pid}).

lookup(Meter_type, Meter) ->
    case ets:lookup(?TABLE_ID, {Meter_type, Meter}) of
        [{{Meter_type, Meter}, Pid}] -> 
            {ok, Pid};
        [] -> 
            {error, not_found}
    end.

lookup_by_pid(Pid) ->
    case ets:match_object(?TABLE_ID, {'$1', Pid}) of
        [{{Meter_type, Meter}, Pid}|_] ->
            {ok, {Meter_type, Meter}};
        [] -> 
            {error, not_found}
    end.

delete({Meter_type, Meter} = Key) ->
    ets:delete(?TABLE_ID, Key).

get_running_pid(Meter_type, Meter) ->
    case lookup(Meter_type, Meter) of
        {ok, Pid} ->
            case is_process_alive(Pid) of
                true ->
                    {ok, Pid};
                false ->
                    {error, not_alive}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

select_meter_by_meter_type(Meter_type) ->
    ets:match(?TABLE_ID,{{Meter_type, '$1'},'_'}).

get_all_meter_type_and_meter() ->
    ets:match(?TABLE_ID, {'$1', '_'}).





-module (analyze_meter_allnighter).

-include("analyze.hrl").
-include("analyze_config.hrl").


-export ([is_allnighter/2, update_all_nighter_push_strategy_list/4]).

is_allnighter(Datetime = {_, Time}, Active_power) ->
    Is_in_allnighter_time = (Time >= ?ALLNIGHTER_START_TIME),
    Is_starting_up = (Active_power > ?STARTING_UP_ACTIVE_POWER),
    case {Is_in_allnighter_time, Is_starting_up} of
        {true, true} ->
            true;
        _ ->
            false
    end.

update_all_nighter_push_strategy_list(All_nighter_push_strategy_list, Datetime, Meter_type, Meter) ->
    {_, {Hour, _, _}} = Datetime,
    case lists:keyfind(Hour, 1, All_nighter_push_strategy_list) of
        {Hour, 0} -> 
            push_msg(Meter_type, Meter),
            lists:keyreplace(Hour, 1, All_nighter_push_strategy_list, {Hour, 1});
        _ ->
            All_nighter_push_strategy_list
    end.

push_msg(Meter_type, Meter) ->
    Msg_type = "Work_all_night",
    Msg_body = get_msg_body(Meter_type, Meter),
    analyze_push_service:push(Msg_type, Msg_body),
    ok.

get_msg_body(Meter_type, Meter) ->
    string:join([Meter_type, Meter], "#").

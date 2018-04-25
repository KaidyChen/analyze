-module (analyze_build_time_table).

-include("analyze.hrl").
-include("analyze_config.hrl").
-include("print.hrl").


-export([add_time_table/1]).

%% 0000000001#0001000100020002#0a0001aa7k&ac;0a0001aa8k&lighting#1#2016-08-21#2017-01-20#1#1&08:00-12:00 13:00-18:00 18:40-21:30;2&08:00-12:00 13:00-18:00 18:40-21:30
add_time_table(Data_field_str) ->
    case string:tokens(Data_field_str, "#") of
        [Task_id, Room_id, Type_and_labels_of_meter, Level_str, Validity_date_start_str, Validity_date_end_str, Holiday_mode_str, Time_list_str] ->
            Validity_date_start = ?HELP:strToDate(Validity_date_start_str),
            Validity_date_end = ?HELP:strToDate(Validity_date_end_str),
            Holiday_mode = list_to_integer(Holiday_mode_str),
            Level = list_to_integer(Level_str),
            case is_validity_date(Validity_date_start, Validity_date_end) of
                true ->
                    Type_and_labels_of_meter_list = get_type_and_labels_list(Type_and_labels_of_meter),
                    Time_list = get_time_list(Time_list_str),
                    Time_table_field = {Room_id, Type_and_labels_of_meter_list, Level, Validity_date_start, Validity_date_end, Holiday_mode, Time_list},
                    analyze_server:add_time_table(Time_table_field);
                false ->
                    ?ERROR("is_validity_date(~p, ~p) is error.~n", [Validity_date_start, Validity_date_end])
            end;
        _ ->
            ?ERROR("Data_field_str:~p of add_time_table is not match~n", [Data_field_str])
    end.

is_validity_date(Validity_date_start, Validity_date_end) ->
    (Validity_date_start =< Validity_date_end).

get_type_and_labels_list(Type_and_labels_of_meter) ->
    Type_and_labels_list = lists:map(fun
        (Type_and_label) ->
            string:tokens(Type_and_label, "&")
    end, string:tokens(Type_and_labels_of_meter, ";")),
    Type_and_labels_list.

get_time_list(Time_list_str) ->
    Time_tables_list = lists:map(fun
        (Time_field) ->
            [Day_of_the_week_str, Time_str] = string:tokens(Time_field, "&"),
            Time_list_of_day = lists:map(fun
                (Time) ->
                    [Time_1, Time_2] = string:tokens(Time, "-"),
                    [Hour_1, Minute_1] = string:tokens(Time_1, ":"),
                    [Hour_2, Minute_2] = string:tokens(Time_2, ":"),
                    {{list_to_integer(Hour_1), list_to_integer(Minute_1)}, {list_to_integer(Hour_2), list_to_integer(Minute_2)}}
            end, string:tokens(Time_str, ?FS)),
            {list_to_integer(Day_of_the_week_str), Time_list_of_day}
    end, string:tokens(Time_list_str, ";")),
    Time_tables_list.

-module (analyze_central_ac_frozen_data_of_month).

-include("analyze.hrl").
-include("analyze_config.hrl").
-include("print.hrl").
-include("cmd_obj.hrl").

-define(CMD_TYPE, "dsj").
-define(CMD_ID, "syyktdjsj").

-export([start/1, 
         parse_result/1, 
         get_frozen_data_last_year_month/3,
         get_frozen_data_of_year_month/2,
         is_can_write/2, 
         save_hour_data/3
]).

is_can_write({Last_year, Last_month}, {Data_year, Data_month}) when (Data_year =:= Last_year) andalso (Data_month =< 12) andalso (Data_month > Last_month) ->
    true;
is_can_write({Last_year, Last_month}, {Data_year, Data_month}) when (Data_year =:= Last_year+1) andalso (Data_month =< 12) ->
    true;
is_can_write(_, _) ->
    false.
    
start({Meter_type, Meter, Gateway, Cq_weight, Frozen_data_last_year_month = {Last_year, Last_month}, Report_time = {Date, _}}) ->
    {Year, Month, _} = Date,
    case how_monthly_interval(Frozen_data_last_year_month, {Year, Month}) of
        0 -> ok;
        N when (N >= 1) andalso (N =< 12) -> 
            complementary_data(Meter_type, Meter, Gateway, Cq_weight, N);
        N when (N > 12) ->
            complementary_data(Meter_type, Meter, Gateway, Cq_weight, 12)
    end.

get_frozen_data_last_year_month(Datetime_now, Meter_type, Meter) ->
    {{Year, Month, _} = Date, _} = Datetime_now,
    Prev_year_month = ?HELP:get_prev_month(Date),
    % Prev_year_month = {2016, 9},
    case get_frozen_data_last_year_month_filepath(Year, Meter) of
        {ok, Filepath} ->
            case get_frozen_data_last_year_month_0(Filepath) of
                {ok, {Last_year, Last_month}} ->
                    {Last_year, Last_month};
                _ ->
                    Prev_year_month
            end;
        {error, Reason} ->
            Prev_year_month
    end.

get_frozen_data_of_year_month({Year, Month}, Meter) ->
    Filepath = analyze_util:get_frozen_data_of_month_filepath(Year, Meter),
     case file:read_file(Filepath) of
        {ok, Binary} ->
            Data_list = binary_to_list(Binary),
            Data_line_list = string:tokens(Data_list, ?NL),
            Year_month_str = ?HELP:year_and_month_str(Year, Month),
            get_frozen_data_of_year_month_0(Data_line_list,Year_month_str);
        {error, Reason} ->
            {error, Reason}
    end.

get_frozen_data_of_year_month_0([Data_line | List], Year_month_str) ->
    case lists:prefix(Year_month_str, Data_line) of
        true ->
            Data_item_list = string:tokens(Data_line, ?FS),
            {ok, Data_item_list};
        false ->
            get_frozen_data_of_year_month_0(List, Year_month_str)
    end;
get_frozen_data_of_year_month_0(_, _) ->
    {error, not_found}.

%% 解析返回的结果
parse_result(Result_str_tmp) ->
    case string:tokens(Result_str_tmp, ",") of
        [Frozen_time_str, Low_speed_used_time_str, Medium_speed_used_time_str, High_speed_used_time_str, Low_speed_used_amount_str, Medium_speed_used_amount_str, High_speed_used_amount_str] ->
            Year_data_str = string:sub_string(Frozen_time_str, 1, 2),
            Month_data_str = string:sub_string(Frozen_time_str, 3, 4),
            Day_data_str = string:sub_string(Frozen_time_str, 5, 6),
            Hour_data_str = string:sub_string(Frozen_time_str, 7, 8),
            Year_data = list_to_integer(Year_data_str) + 2000,
            Month_data = list_to_integer(Month_data_str),
            Day_data = list_to_integer(Day_data_str),
            Hour_data = list_to_integer(Hour_data_str),
            Date = {Year_data, Month_data, Day_data},
            Time = {Hour_data, 0, 0},
            Date_str = ?HELP:dateToStr(Date),
            Time_str = ?HELP:timeToStr(Time),
            Data_item_list = [Date_str, Time_str, Low_speed_used_time_str, Medium_speed_used_time_str, High_speed_used_time_str, Low_speed_used_amount_str, Medium_speed_used_amount_str, High_speed_used_amount_str],
            {ok, {Year_data, Month_data}, Data_item_list};
        _ ->
            {error, not_valid_data}
    end.

save_hour_data({Year, Month}, Meter, Data_item_list) ->
    Year_month_str = ?HELP:year_and_month_str(Year, Month),
    Filepath = analyze_util:get_frozen_data_of_month_filepath(Year, Meter),
    Content = string:join(Data_item_list, ?FS),
    ?ERROR("~p~n", [Content]),
    save_hour_data_(Filepath, Content),
    ok.

save_hour_data_(Filepath, Append_content) ->
    filelib:ensure_dir(Filepath),
    ?HELP:append_content(Filepath, Append_content).

get_frozen_data_last_year_month_0(Filepath) ->
    case file:read_file(Filepath) of
        {ok, Binary} ->
            Data_list = binary_to_list(Binary),
            Data_line_list = string:tokens(Data_list, ?NL),
            [Last_line | _] = lists:reverse(lists:sort(Data_line_list)),
            [Date_str, Time_str, _Low_speed_used_time_str, _Medium_speed_used_time_str, _High_speed_used_time, _Low_speed_used_amount_str, _Medium_speed_used_amount_str, _High_speed_used_amount_str] = string:tokens(Last_line, ?FS),
            {Year, Month, _} = ?HELP:strToDate(Date_str),
            {ok, {Year, Month}};
        {error, Reason} ->
            {error, Reason}
    end.

complementary_data(Meter_type, Meter, Gateway, Cq_weight, N) when (N > 0) ->
    case analyze_gateway_util:get_gateway_pid_by_gateway(Gateway) of
        {ok, Pid} ->
            build_send_cmd_obj(Pid, N, Meter_type, Meter, self());
        {error, Reason} ->
            ?ERROR("get_gateway_pid_by_gateway(~p) is error:~p~n", [Gateway, Reason])
    end.

build_send_cmd_obj(Pid, N, Meter_type, Meter, Self) when (N > 0) ->
    Cmd_obj_list = [get_cmd_obj(X, Meter_type, Meter) || X <- lists:reverse(lists:seq(1, N))],
    Pid ! {frozen_data_of_month, Self, Meter_type, Meter, Cmd_obj_list};
build_send_cmd_obj(_, _, _, _, _) ->
    ok.

get_cmd_obj(N, Meter_type, Meter) ->
    #cmd_obj{
        eqpt_type = Meter_type, 
        eqpt_id_code = Meter, 
        cmd_type = ?CMD_TYPE, 
        cmd_id = ?CMD_ID, 
        cmd_data = integer_to_list(N)
    }.

%% 间隔多少月
how_monthly_interval({Year_min, Month_min}, {Year_max, Month_max}) when (Year_max =:= Year_min) andalso (Month_max >= Month_min) ->
    Month_max - Month_min;
how_monthly_interval({Year_min, Month_min}, {Year_max, Month_max}) when(Year_max > Year_min) ->
    (Year_max - Year_min) * 12 + Month_max - Month_min;
how_monthly_interval(_, _) ->
    erlang:error(badarg).

get_frozen_data_last_year_month_filepath(Year, Meter) ->
    Filepath1 = analyze_util:get_frozen_data_of_month_filepath(Year, Meter),
    case filelib:file_size(Filepath1) > 0 of
        true ->
            {ok, Filepath1};
        false ->
            Filepath2 = analyze_util:get_frozen_data_of_month_filepath(Year-1, Meter),
            case filelib:file_size(Filepath2) > 0 of
                true ->
                    {ok, Filepath2};
                false ->
                    {error, enoent}
            end
    end.


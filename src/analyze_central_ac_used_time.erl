-module (analyze_central_ac_used_time).

-include("print.hrl").
-include("analyze.hrl").
-include("analyze_config.hrl").

-export([start/1,
         get_last_record_date_time/2,
         save_data/1
]).

start({Meter_type, Meter, Low_speed_used_time_str, Medium_speed_used_time_str, High_speed_used_time_str, Datetime_last, {{Year, Month, _Day}, _Time} = Datetime_now}) ->
        Hour_diff = ?HELP:how_hourly_interval(Datetime_last, Datetime_now),
        case Hour_diff > 0 of
            true -> 
                %% 相差一个小时，且当前是整点, 向自身发送信息
                self() ! {hour_data_on_the_hour, Datetime_now, Low_speed_used_time_str, Medium_speed_used_time_str, High_speed_used_time_str};
            false -> ok
        end.

get_last_record_date_time({{Year, Month, _Day}, _Time} = Datetime_now, Meter) ->
    Prev_hour_datetime = ?HELP:addHour(Datetime_now, -1),
    case get_last_record_of_used_time(Year, Month, Meter) of
        {ok, Last_record_line} ->
            case string:tokens(Last_record_line, ?FS) of
                [Date_str, Time_str, _, _, _] ->
                    Date_last = ?HELP:strToDate(Date_str),
                    Time_last = ?HELP:strToTime(Time_str),
                    {Date_last, Time_last};             
                _ -> 
                    Prev_hour_datetime
            end;
        {error, _} ->
           Prev_hour_datetime
    end.

get_last_record_of_used_time(Year, Month, Meter) ->
    FilePath = get_used_time_file_path(Year, Month, Meter),
    case file:read_file(FilePath) of
        {ok, Binary_1} -> {ok, get_last_record(Binary_1)};
        {error, enoent} -> 
            {Tmp_year, Tmp_month} = ?HELP:get_prev_month(Year, Month),
            Prev_month_filePath = get_used_time_file_path(Tmp_year, Tmp_month, Meter),
            case file:read_file(Prev_month_filePath) of
                {ok, Binary_2} -> {ok, get_last_record(Binary_2)};
                {error, enoent} -> {error, enoent}; 
                {error, Reason_2} -> {error, Reason_2}  
            end;
        {error, Reason_1} -> {error, Reason_1}
    end.

get_last_record(Data_Bin) ->
    Data_List = binary_to_list(Data_Bin),
    DataLine_List = string:tokens(Data_List, ?NL),
    lists:last(DataLine_List).

save_data({Datetime_now, Meter, Low_speed_used_time_str, Medium_speed_used_time_str, High_speed_used_time_str}) ->
    case save_data_(Datetime_now, Meter, Low_speed_used_time_str, Medium_speed_used_time_str, High_speed_used_time_str) of
        ok              -> ok;
        {error, Reason} -> ?ERROR("save_hour_data Meter:~p is error:~p~n", [Meter, file:format_error(Reason)])
    end.

save_data_({Date, Time}, Meter, Low_speed_used_time_str, Medium_speed_used_time_str, High_speed_used_time_str) ->
    {Year, Month, _Day} = Date,
    FilePath = get_used_time_file_path(Year, Month, Meter),
    filelib:ensure_dir(FilePath),
    Datetime_str = ?HELP:getDateTimeStr(Date, Time),
    Append_content = string:join([Datetime_str, Low_speed_used_time_str, Medium_speed_used_time_str, High_speed_used_time_str], ?FS),
    case save_to_file(FilePath, Append_content) of
        ok              -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% 获取整点数据文件路径
get_used_time_file_path(Year, Month, Meter) ->
    ?HELP:get_year_month_file_path(Year, Month, Meter, ?CENTRAL_AC_USED_TIME).

save_to_file(FilePath, Append_content) ->
    ?HELP:append_content(FilePath, Append_content).


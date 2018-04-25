-module (analyze_48hour_central_ac_used_time).

-include("print.hrl").
-include("analyze.hrl").
-include("analyze_config.hrl").

-export([start/2, get_48hour_used_time_filepath/1]).

-define(USED_TIME_48HOUR, "48hour_used_time").
-define(HOW_HOUR, 48).

start(DateTime, Meter) ->
    {{Year, Month, _Day}, _Time} = DateTime,
    case get_dataLine_list(Year, Month, Meter) of
        {ok, DataLine_list} ->
            Processed_list = lists:filtermap(fun process_DataLine/1, DataLine_list),
            Length = length(Processed_list),
            New_processed_list = case (Length >= ?HOW_HOUR) of
                false ->
                    %% 不足?HOW_HOUR条记录，再读取上一个月的数据
                    {Tmp_year, Tmp_month} = ?HELP:get_prev_month(Year, Month),
                    Tmp_prev_list = case get_dataLine_list(Tmp_year, Tmp_month, Meter) of
                        {ok, Prev_dataLine_list} ->
                            Prev_processed_list = lists:filtermap(fun process_DataLine/1, Prev_dataLine_list),
                            Prev_length = length(Prev_processed_list),
                            case (Prev_length >= (?HOW_HOUR-Length)) of
                                true ->
                                    lists:nthtail(Prev_length-(?HOW_HOUR-Length), Prev_processed_list);
                                false ->
                                    Prev_processed_list
                            end; 
                        {error, enoent} -> [];
                        {error, Reason1} ->
                            ?ERROR("Meter:~p get_dataLine_list is error:~p~n", [Meter, Reason1]),
                            []
                    end,
                    lists:append(Tmp_prev_list, Processed_list);
                true ->
                    %% 只截取?HOW_HOUR条记录
                    lists:nthtail(Length-?HOW_HOUR, Processed_list)
            end,
            Data_line_list = cal_hour_used_time(New_processed_list),
            DataPoint_Content = string:join(Data_line_list, ?NL),
            SaveFilePath = get_48hour_used_time_filepath(Meter),
            case ?CALHELP:writeToFile(SaveFilePath, DataPoint_Content) of
                ok ->
                    ?PRINT("~s is update~n", [SaveFilePath]);
                {error, Reason2} ->
                    ?ERROR("writeToFile ~p is error : ~p~n", [SaveFilePath, file:format_error(Reason2)])
            end, 
            ok;
        {error, Reason} ->  
            ?ERROR("Meter:~p get_dataLine_list is error:~p~n", [Meter, Reason])
    end,
    ok.

%% 读取表每月小时电量数据
get_dataLine_list(Year, Month, Meter) ->
    FilePath = ?HELP:get_year_month_file_path(Year, Month, Meter, ?CENTRAL_AC_USED_TIME),
    case file:read_file(FilePath) of
        {ok, Data_Bin} -> 
            Data_List = binary_to_list(Data_Bin),
            DataLine_List = string:tokens(Data_List, ?NL),
            {ok, DataLine_List};
        {error, Reason} -> 
            {error, Reason}
    end.

process_DataLine(Data_line) ->
    case string:tokens(Data_line, ?FS) of
        [Date_str, Time_str, Low_speed_used_time_str, Medium_speed_used_time_str, High_speed_used_time_str] ->
            Date = ?HELP:strToDate(Date_str),
            Time = ?HELP:strToTime(Time_str),
            Datetime = {Date, Time},
            All_speed_used_time_int = list_to_integer(Low_speed_used_time_str) + list_to_integer(Medium_speed_used_time_str) + list_to_integer(High_speed_used_time_str),
            {true, {Datetime, All_speed_used_time_int}};
        _ ->
            false
    end.

cal_hour_used_time([H | T]) ->
    Func = fun(Next, {Current, Hour_used_time_list}) ->
            {Next_datetime, Next_used_time} = Next,
            {Current_datetime, Current_used_time} = Current,
            Current_datetime_str = ?HELP:getDateTimeStr(Current_datetime),
            Next_datetime_str = ?HELP:getDateTimeStr(Next_datetime),
            Used_time_str = integer_to_list(Next_used_time - Current_used_time),
            New_Hour_used_time_list = [string:join([Current_datetime_str, Next_datetime_str, Used_time_str], ?FS) | Hour_used_time_list],
            {Next, New_Hour_used_time_list}
    end,
    {_, Hour_used_time_line_list} = lists:foldl(Func, {H, []}, T),
    lists:reverse(Hour_used_time_line_list).

get_48hour_used_time_filepath(Meter) ->
    SaveFileName = lists:concat([?USED_TIME_48HOUR, ?SUFFIX]),
    filename:join([?METERDIR, Meter, SaveFileName]).
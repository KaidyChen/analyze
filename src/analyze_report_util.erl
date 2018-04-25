-module(analyze_report_util).

-include("analyze.hrl").
-include("analyze_config.hrl").

-compile([export_all]).


get_meter_first_and_last_report_filepath_of_month(Meter, Year, Month) ->
    case get_frist_and_last_day_of_month_filepath(Meter, Year, Month) of
        {ok, {Frist_filepath, Last_filepath}} ->
            {Year_prev, Month_prev} = ?HELP:get_prev_month(Year, Month),
            case get_frist_and_last_day_of_month_filepath(Meter, Year_prev, Month_prev) of
                {ok, {Frist_filepath_prev, Last_filepath_prev}} ->
                    {ok, {1, Last_filepath_prev, Last_filepath}};
                {error, What} ->
                    {ok, {2, Frist_filepath, Last_filepath}}
            end;
        {error, Reason} -> {error, Reason}
    end.

%% 获取年月中第一日和最后一日的路径名
get_frist_and_last_day_of_month_filepath(Meter, Year, Month) ->
    Year_str = integer_to_list(Year),
    Month_str = integer_to_list(Month),
    Month_dir = filename:join([?METERDIR, Meter, Year_str, Month_str]),
    case file:list_dir_all(Month_dir) of
        {ok, []} -> {error, "no files"};
        {ok, Filename_list} ->
            Filename_sorted_list = lists:sort(Filename_list),
            [Frist_filename | _] = Filename_sorted_list,
            [Last_filename | _] = lists:reverse(Filename_sorted_list),
            Frist_filepath = filename:join(Month_dir, Frist_filename),
            Last_filepath = filename:join(Month_dir, Last_filename),
            {ok, {Frist_filepath, Last_filepath}};
        {error, Reason} -> {error, Reason}
    end.

get_last_line(Filepath) ->
    case file:read_file(Filepath) of
        {ok, Data_bin} ->
            Data_list = binary_to_list(Data_bin),
            [Last_line | _] = lists:reverse(string:tokens(Data_list, ?NL)),
            {ok, Last_line};
        {error, Reason} -> {error, Reason}
    end.

get_frist_line(Filepath) ->
    case file:read_file(Filepath) of
        {ok, Data_bin} ->
            Data_list = binary_to_list(Data_bin),
            [Last_line | _] = string:tokens(Data_list, ?NL),
            {ok, Last_line};
        {error, Reason} -> {error, Reason}
    end.

%% 获取上报的报文要写入的文件路径
%% Msg_type/Year/Month/Year-Month-Day + SUFFIX
get_meter_datamsg_filepath(Meter, {Date, _Time}) ->
    get_meter_datamsg_filepath(Meter, Date);
get_meter_datamsg_filepath(Meter, Date = {Year, Month, Day}) -> 
    Date_str = ?HELP:dateToStr(Date, "-"),
    Filename = lists:concat([Date_str, ?SUFFIX]),
    Year_str = integer_to_list(Year),
    Month_str = integer_to_list(Month),
    filename:join([?METERDIR, Meter, Year_str, Month_str, Filename]).

%% 时间戳加上内容写入文件
%% Msg_filepath:文件路径
%% Content:文件内容
save_datetime_and_Content(FilePath, Content, Datetime) when is_tuple(Datetime) ->
    DateTime_str = ?HELP:getDateTimeStr(Datetime),
    case file:open(FilePath, [write, binary, append]) of
        {ok, Fd} ->
            io:fwrite(Fd, "~s ~s~n", [DateTime_str, Content]),
            file:close(Fd),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.


%% 根据设备类型，得到上报数据中的各项

%% 分体空调
%% 日期 时间 电量 电压 电流 功率 温度 频率 因子 继电器 状态字 设备日期 设备时间
%% 2016-09-29 09:06:47 000033.28 226.8 004.087 895.6 028.5 49.96 0.968 00 000200 2016-09-29 09:06:00

%% 插座
%% 日期 时间 电量 电压 电流 功率 频率 因子 继电器 状态字 设备日期 设备时间
%% 2016-09-29 09:22:42 000001.77 226.2 000.356 45.9 49.97 0.564 00 000000 2016-02-04 07:08:00

%% 中央空调
%% 日期 时间 电量 功率 温度 继电器 温度及模式 风速档位 低速使用时长 中速使用时长 高速使用时长 剩余金额 状态字 设备日期 设备时间
%% 2016-09-29 09:15:38 000138.94 0.0 027.0 01 2600 03 00012837 00008221 00037596 050077.85 000000 2016-09-29 09:15:00

%% 电量
get_ele(Type, Data_line) when Type =:= ?SOCKET_TYPE; Type =:= ?FOUR_WAY_SWITCH_TYPE;
                                            Type =:= ?LIGHTING_TYPE ->
    [_, _, Ele | _] = string:tokens(Data_line, ?FS),
    list_to_float(Ele);
get_ele(?AC_TYPE, Data_line) ->
    [_, _, Ele | _] = string:tokens(Data_line, ?FS),
    list_to_float(Ele);
get_ele(?CENTRAL_AC_TYPE, Data_line) ->
    [_Report_date, _Report_time, Electric_power, _Active_power, _Temp, _Relay_status, _Temp_and_mode, _Wind_speed_gears, _Low_speed_used_time, _Medium_speed_used_time, _High_speed_used_time, _Amount, _Status_word, _Meter_date, _Meter_time] = string:tokens(Data_line, ?FS),
    list_to_float(Electric_power).

%% 温度
get_temp(?AC_TYPE, Data_line) ->
    [_, _, _, _, _, _, Temp_str | _] = string:tokens(Data_line, ?FS),
    Temp_str;
get_temp(?CENTRAL_AC_TYPE, Data_line) ->
    [_Report_date, _Report_time, _Electric_power, _Active_power, Temp, _Relay_status, _Temp_and_mode, _Wind_speed_gears, _Low_speed_used_time, _Medium_speed_used_time, _High_speed_used_time, _Amount, _Status_word, _Meter_date, _Meter_time] = string:tokens(Data_line, ?FS),
    Temp.

get_temp_float(?AC_TYPE, Data_line) ->
    [_, _, _, _, _, _, Temp_str | _] = string:tokens(Data_line, ?FS),
    list_to_float(Temp_str);
get_temp_float(?CENTRAL_AC_TYPE, Data_line) ->
    [_Report_date, _Report_time, _Electric_power, _Active_power, Temp, _Relay_status, _Temp_and_mode, _Wind_speed_gears, _Low_speed_used_time, _Medium_speed_used_time, _High_speed_used_time, _Amount, _Status_word, _Meter_date, _Meter_time] = string:tokens(Data_line, ?FS),
    list_to_float(Temp).


%% 上报的时间 "2016-09-23 16:13:14"
get_datetime_str(Type, Data_line) when Type =:= ?SOCKET_TYPE; Type =:= ?FOUR_WAY_SWITCH_TYPE;
                                            Type =:= ?LIGHTING_TYPE ->
    [Date_str, Time_str | _] = string:tokens(Data_line, ?FS),
    string:join([Date_str, Time_str], ?FS);
get_datetime_str(?AC_TYPE, Data_line) ->
    [Date_str, Time_str | _] = string:tokens(Data_line, ?FS),
    string:join([Date_str, Time_str], ?FS);
get_datetime_str(?CENTRAL_AC_TYPE, Data_line) ->
    [Report_date, Report_time, _Electric_power, _Active_power, _Temp, _Relay_status, _Temp_and_mode, _Wind_speed_gears, _Low_speed_used_time, _Medium_speed_used_time, _High_speed_used_time, _Amount, _Status_word, _Meter_date, _Meter_time] = string:tokens(Data_line, ?FS),
    string:join([Report_date, Report_time], ?FS).

%% 上报的时间 {{2016, 9, 23}, {16, 13, 14}}
get_datetime_tuple(Type, Data_line) when Type =:= ?SOCKET_TYPE; Type =:= ?FOUR_WAY_SWITCH_TYPE;
                                            Type =:= ?LIGHTING_TYPE ->
    [Date_str, Time_str | _] = string:tokens(Data_line, ?FS),
    Date = ?HELP:strToDate(Date_str, "-"),
    Time = ?HELP:strToTime(Time_str, ":"),
    {Date, Time};
get_datetime_tuple(?AC_TYPE, Data_line) ->
    [Date_str, Time_str | _] = string:tokens(Data_line, ?FS),
    Date = ?HELP:strToDate(Date_str, "-"),
    Time = ?HELP:strToTime(Time_str, ":"),
    {Date, Time};
get_datetime_tuple(?CENTRAL_AC_TYPE, Data_line) ->
    [Report_date, Report_time, _Electric_power, _Active_power, _Temp, _Relay_status, _Temp_and_mode, _Wind_speed_gears, _Low_speed_used_time, _Medium_speed_used_time, _High_speed_used_time, _Amount, _Status_word, _Meter_date, _Meter_time] = string:tokens(Data_line, ?FS),
    Date = ?HELP:strToDate(Report_date, "-"),
    Time = ?HELP:strToTime(Report_time, ":"),
    {Date, Time}.

%% 功率
get_active_power(Type, Data_line) when Type =:= ?SOCKET_TYPE; Type =:= ?FOUR_WAY_SWITCH_TYPE;
                                            Type =:= ?LIGHTING_TYPE ->
    [_, _, _, _, _, Active_power_str | _] = string:tokens(Data_line, ?FS),
    list_to_float(Active_power_str);
get_active_power(?AC_TYPE, Data_line) ->
    [_, _, _, _, _, Active_power_str | _] = string:tokens(Data_line, ?FS),
    list_to_float(Active_power_str);
get_active_power(?CENTRAL_AC_TYPE, Data_line) ->
    [_Report_date, _Report_time, _Electric_power, Active_power, _Temp, _Relay_status, _Temp_and_mode, _Wind_speed_gears, _Low_speed_used_time, _Medium_speed_used_time, _High_speed_used_time, _Amount, _Status_word, _Meter_date, _Meter_time] = string:tokens(Data_line, ?FS),
    list_to_float(Active_power).

%% 中央空调的中高低速使用时长之和
get_used_time_sum(?CENTRAL_AC_TYPE, Data_line) ->
    lists:sum(get_three_speed_used_time(?CENTRAL_AC_TYPE, Data_line)).

get_three_speed_used_time(?CENTRAL_AC_TYPE, Data_line) ->
    [_Report_date, _Report_time, _Electric_power, Active_power, _Temp, _Relay_status, _Temp_and_mode, _Wind_speed_gears, Low_speed_used_time, Medium_speed_used_time, High_speed_used_time, _Amount, _Status_word, _Meter_date, _Meter_time] = string:tokens(Data_line, ?FS),    
    [list_to_integer(X) || X <- [Low_speed_used_time, Medium_speed_used_time, High_speed_used_time]].




-module(analyze_hour_avg_usedele).

-include("analyze.hrl").
-include("analyze_config.hrl").
-include("print.hrl").

-export([start/3, get_avg_hour_usedele_filepath/3]).

-define(LACKPOINTTIMEDIFF, (15*60)).
-define(STANDARDPOWER, 1.4).

start(Meter_type, Meter, Datetime) ->
    Datamsg_filepath = analyze_report_util:get_meter_datamsg_filepath(Meter, Datetime),
    case get_datetime_electric_power(Datamsg_filepath) of
        {ok, New_data_line_list} ->
            {Used_ele_all_time, All_used_ele} = cal_used_time_and_ele(New_data_line_list),
            Avg_hour_used_ele = cal_avg_hour_user_ele(Used_ele_all_time, All_used_ele),
            Avg_hour_used_ele_str = float_to_list(Avg_hour_used_ele, [{decimals, 2}, compact]),
            case update_avg_hour_used_ele(Meter, Datetime, Avg_hour_used_ele_str) of
                ok -> ok;
                {error, Reason_1} -> 
                    ?ERROR("~p update_avg_hour_used_ele is error: ~p", [?MODULE, Reason_1])
            end;
        {error, Reason} ->
            ?ERROR("~p get_datetime_electric_power(~p) is error: ~p", [?MODULE, Datamsg_filepath, Reason])
    end.

get_datetime_electric_power(Datamsg_filepath) ->
    case file:read_file(Datamsg_filepath) of
        {ok, Data_bin} ->
            Data_bitstring = binary_to_list(Data_bin),
            Data_line_list = string:tokens(Data_bitstring, ?NL),
            Fun = fun
                (Data_line) ->
                    %% 2016-09-18 14:38:37 001294.02 214.3 006.476 1366.9 026.5 49.98 0.985 00 04?ONE_HOUR_SECONDS 2016-09-18 14:38:00
                    [Date_str, Time_str, Electric_power_str | _] = string:tokens(Data_line, ?FS),
                    Date = ?HELP:strToDate(Date_str, "-"),
                    Time = ?HELP:strToTime(Time_str, ":"),
                    Datetime = {Date, Time},
                    Electric_power_float = list_to_float(Electric_power_str),
                    New_data_line = {Datetime, Electric_power_float},
                    {true, New_data_line}
            end,
            New_data_line_list = lists:filtermap(Fun, lists:usort(Data_line_list)),
            {ok, New_data_line_list};
        {error, Reason} -> {error, Reason}
    end.

cal_used_time_and_ele(Data_line_list) ->
    cal_used_time_and_ele_(Data_line_list, 0, 0.0).

cal_used_time_and_ele_([Data_cur | T], Used_ele_all_time, All_used_ele) when (T =/= []) ->
    [Data_next | _] = T,
    {Datetime_cur, Electric_power_cur} = Data_cur,
    {Datetime_next, Electric_power_next} = Data_next,
    Electric_power_diff = ?HELP:floatDecimal((Electric_power_next-Electric_power_cur), 2),

    Second_diff = ?HELP:getTimeDiff(Datetime_cur, Datetime_next),

    if
        (Electric_power_diff =< ?USEDELEDIFF) -> 
            cal_used_time_and_ele_(T, Used_ele_all_time, All_used_ele);
        (Electric_power_diff > ?USEDELEDIFF) andalso (Second_diff > ?LACKPOINTTIMEDIFF) ->
            Time_Diff = round(Electric_power_diff / ?STANDARDPOWER * ?ONE_HOUR_SECONDS),
            cal_used_time_and_ele_(T, Used_ele_all_time+Time_Diff, All_used_ele+Electric_power_diff);
        (Electric_power_diff > ?USEDELEDIFF) andalso (Second_diff =< ?LACKPOINTTIMEDIFF) ->
             cal_used_time_and_ele_(T, Used_ele_all_time+Second_diff, All_used_ele+Electric_power_diff)
    end;
cal_used_time_and_ele_(_, Used_ele_all_time, All_used_ele) ->
    {Used_ele_all_time, All_used_ele}.

cal_avg_hour_user_ele(Used_ele_all_time, All_used_ele) ->
    case (Used_ele_all_time =:= 0) of
        true -> 0.0;
        false -> 
            Avg_hour_user_ele = All_used_ele / (Used_ele_all_time / ?ONE_HOUR_SECONDS),
            Avg_hour_user_ele
    end.

%% 获取平均小时用电量的的文件路径
get_avg_hour_usedele_filepath(Year, Month, Meter) ->
    ?HELP:get_year_month_file_path(Year, Month, Meter, ?HOUR_AVG_USES_ELE).

update_avg_hour_used_ele(Meter, Datetime = {Date, _}, Avg_hour_used_ele_str) ->
    {Year, Month, _} = Date,
    Avg_hour_usedele_filepath = get_avg_hour_usedele_filepath(Year, Month, Meter),
    case get_old_avg_hour_used_ele_list(Avg_hour_usedele_filepath) of
        {ok, Old_avg_hour_used_ele_list} ->
            Date_Str = ?HELP:dateToStr(Date, "-"),
            Old_avg_hour_used_ele_list_tmp = [Item || Item <- Old_avg_hour_used_ele_list, lists:prefix(Date_Str, Item) =:= false],
            Today_avg_hour_user_ele = string:join([Date_Str, Avg_hour_used_ele_str], ?FS),
            New_avg_hour_used_ele_list_tmp = lists:reverse([Today_avg_hour_user_ele | lists:reverse(lists:sort(Old_avg_hour_used_ele_list_tmp))]),
            New_content = string:join(New_avg_hour_used_ele_list_tmp, ?NL),
            update_file(Avg_hour_usedele_filepath, New_content);
        {error, Reason} ->
            ?ERROR("~p get_old_avg_hour_used_ele_list is error: ~p", [?MODULE, Reason])            
    end.

get_old_avg_hour_used_ele_list(Avg_hour_usedele_filepath) ->
    case file:read_file(Avg_hour_usedele_filepath) of
        {ok, Data_bin} ->
            Data_list = binary_to_list(Data_bin),
            {ok, string:tokens(Data_list, ?NL)};
        {error, enoent} -> {ok, []};
        {error, Reason} -> {error, Reason}
    end.

update_file(Filepath, Content) ->
    filelib:ensure_dir(Filepath),
    case file:open(Filepath, [write, binary]) of
        {ok, Fd} ->
            io:fwrite(Fd, "~s~n", [Content]),
            file:close(Fd),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.
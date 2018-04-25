-module (analyze_used_time_of_day).

-include("analyze.hrl").
-include("analyze_config.hrl").
-include("analyze_meter.hrl").
-include("print.hrl").

-export([start/0, start/1]).

start() ->
    start(?HELP:date()).

start(Today) ->
    Date = ?HELP:addDay(Today, -1), 
    Do_fun = fun
        (Meter_type) ->
            All_XX_meter = analyze_meter_util:select_meter_by_meter_type(Meter_type),
            lists:foreach(fun
                ([Meter]) ->
                    do_work(Meter_type, Meter, Date)
            end, All_XX_meter)
    end,
    [Do_fun(Meter_type) || Meter_type <- [?AC_TYPE, ?SOCKET_TYPE, ?FOUR_WAY_SWITCH_TYPE]],
    ok.

do_work(Meter_type, Meter, Date) ->
    Datamsg_filepath = analyze_report_util:get_meter_datamsg_filepath(Meter, Date),
    case get_datetime_electric_active_power(Datamsg_filepath, Meter_type) of
        {ok, New_data_line_list} ->
            Used_time = cal_used_time(New_data_line_list),
            Used_time_of_day_filepath = analyze_util:get_used_time_of_day_filepath(Date, Meter),
            Used_time_str = ?HELP:float_to_decimal_str(Used_time / ?ONE_HOUR_SECONDS, 2),
            Date_str = ?HELP:dateToStr(Date, "-"),
            New_content = string:join([Date_str, Used_time_str], ?FS),
            filelib:ensure_dir(Used_time_of_day_filepath),
            case append_to_file(Used_time_of_day_filepath, New_content) of
                ok -> ok;
                {error, Reason} ->
                    ?ERROR("append_to_file is error:~p", [Reason])
            end,
            ok;
        {error, Reason} ->
            ?ERROR("get_datetime_electric_active_power(~p) is error: ~p", [Datamsg_filepath, Reason])
    end.

get_datetime_electric_active_power(Datamsg_filepath, Meter_type) ->
    case file:read_file(Datamsg_filepath) of
        {ok, Data_bin} ->
            Data_bitstring = binary_to_list(Data_bin),
            Data_line_list = string:tokens(Data_bitstring, ?NL),
            Fun = fun
                (Data_line) ->
                    Datetime = analyze_report_util:get_datetime_tuple(Meter_type, Data_line),
                    Electric_power_float = analyze_report_util:get_ele(Meter_type, Data_line),
                    Active_power_float = analyze_report_util:get_active_power(Meter_type, Data_line),
                    New_data_line = {Datetime, Electric_power_float, Active_power_float},
                    {true, New_data_line}
            end,
            New_data_line_list = lists:filtermap(Fun, lists:usort(Data_line_list)),
            {ok, New_data_line_list};
        {error, Reason} -> {error, Reason}
    end.

cal_used_time(Data_line_list) ->
    cal_used_time_(Data_line_list, 0).

cal_used_time_([Data_cur | T], Used_time_sum) when (T =/= []) ->
    [Data_next | _] = T,
    {Datetime_cur, Electric_power_cur, Active_power_cur} = Data_cur,
    {Datetime_next, Electric_power_next, Active_power_next} = Data_next,
    Electric_power_diff = ?HELP:floatDecimal((Electric_power_next-Electric_power_cur), 2),

    Second_diff = ?HELP:getTimeDiff(Datetime_cur, Datetime_next),

    Used_time = analyze_meter_util:get_used_time(Active_power_cur, Active_power_next, Second_diff, Electric_power_diff),
    cal_used_time_(T, Used_time_sum+Used_time);
    
    % case {Active_power_cur >= ?WORKING_ACTIVE_POWER, Active_power_next >= ?WORKING_ACTIVE_POWER} of
    %     {true, true} ->
    %         cal_used_time_(T, Used_time+Second_diff);
    %     {true, false} ->
    %         %% 0.5h = 1800s
    %         case Second_diff =< 1800 of
    %             true -> cal_used_time_(T, Used_time+round(Electric_power_diff / (Active_power_cur / 1000) * ?ONE_HOUR_SECONDS));
    %             false -> cal_used_time_(T, Used_time)   % 忽略两点之间的时长
    %         end;
    %     {false, true} ->
    %         case Second_diff =< 1800 of
    %             true -> cal_used_time_(T, Used_time+Second_diff);
    %             false -> cal_used_time_(T, Used_time+round(Electric_power_diff / (Active_power_next / 1000) * ?ONE_HOUR_SECONDS))
    %         end;
    %     {false, false} ->
    %         cal_used_time_(T, Used_time)
    % end;
cal_used_time_(_, Used_time) ->
    Used_time.


append_to_file(Filepath, New_content) ->
    case file:open(Filepath, [binary, append]) of
        {ok, Fd} ->
            io:fwrite(Fd, "~s~n", [New_content]),
            file:close(Fd),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

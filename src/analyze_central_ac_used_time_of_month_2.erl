-module (analyze_central_ac_used_time_of_month_2).

-include("analyze.hrl").
-include("analyze_config.hrl").
-include("analyze_meter.hrl").
-include("print.hrl").

-export([start/0, start/1]).

start() ->
    start(?HELP:date()).

start(Date) ->
    {Year, Month, _} = ?HELP:addDay(Date, -1),
    Do_fun = fun
        (Meter_type) ->
            All_XX_meter = analyze_meter_util:select_meter_by_meter_type(Meter_type),
            lists:foreach(fun
                ([Meter]) ->
                    do_work(Meter_type, Meter, Year, Month)
            end, All_XX_meter)
    end,
    [Do_fun(Meter_type) || Meter_type <- [?CENTRAL_AC_TYPE]],
    ok.

do_work(Meter_type, Meter, Year, Month) ->
    case analyze_report_util:get_meter_first_and_last_report_filepath_of_month(Meter, Year, Month) of
        {ok, {1, Last_filepath_prev_month, Last_filepath}} ->
            {ok, Last_line} = analyze_report_util:get_last_line(Last_filepath),
            Last_used_time = analyze_report_util:get_three_speed_used_time (Meter_type, Last_line),
            {ok, Last_line_prev_month} = analyze_report_util:get_last_line(Last_filepath_prev_month),
            Last_used_time_prev_month = analyze_report_util:get_three_speed_used_time(Meter_type, Last_line_prev_month),
            cal_used_time_of_month(Meter, Year, Month, Last_used_time, Last_used_time_prev_month),
            ok;
        {ok, {2, Frist_filepath, Last_filepath}} ->
            {ok, Last_line} = analyze_report_util:get_last_line(Last_filepath),
            Last_used_time = analyze_report_util:get_three_speed_used_time(Meter_type, Last_line),
            {ok, Frist_line} = analyze_report_util:get_frist_line(Frist_filepath),
            Frist_used_time = analyze_report_util:get_three_speed_used_time(Meter_type, Frist_line),
            cal_used_time_of_month(Meter, Year, Month, Last_used_time, Frist_used_time),
            ok;
        {error, Reason} -> 
            ?ERROR("get_meter_first_and_last_report_filepath_of_month(~p, ~p, ~p, ~p) is error: ~p", [Meter_type, Meter, Year, Month, Reason])
    end.

cal_used_time_of_month(Meter, Year, Month, Last_used_time = [Last_low_speed_used_time, Last_medium_speed_used_time, Last_high_speed_used_time], 
                     First_used_time = [Frist_low_speed_used_time, Frist_medium_speed_used_time, Frist_high_speed_used_time]) ->
    Fun = 
        fun(Last_used_time, Frist_used_time) -> 
                ?HELP:float_to_decimal_str((Last_used_time - Frist_used_time)/60, 2)
        end,
    Low_speed_used_time_str = Fun(Last_low_speed_used_time, Frist_low_speed_used_time),
    Medium_speed_used_time_str = Fun(Last_medium_speed_used_time, Frist_medium_speed_used_time),
    High_speed_used_time_str = Fun(Last_high_speed_used_time, Frist_high_speed_used_time),

    Year_and_month_str = ?HELP:year_and_month_str(Year, Month),    
    Used_time_str = string:join([Low_speed_used_time_str, Medium_speed_used_time_str, High_speed_used_time_str], ?FS),
    
    Three_speed_used_time_of_month_filepath = analyze_util:get_three_speed_used_time_of_month_filepath(Year, Meter),
    
    case update_three_speed_used_time(Three_speed_used_time_of_month_filepath, Year_and_month_str, Used_time_str) of
        ok -> ok;
        {error, Reason} ->
            ?ERROR("update_three_speed_used_time is error:~p", [Reason])
    end,
    ok.

update_three_speed_used_time(Three_speed_used_time_of_month_filepath, Year_and_month_str, Used_time_str) ->
    case get_new_used_ele_content(Three_speed_used_time_of_month_filepath, Year_and_month_str, Used_time_str) of
        {ok, New_content} ->
            update_to_file(Three_speed_used_time_of_month_filepath, New_content);
        {error, enoent} ->
            New_content = string:join([Year_and_month_str, Used_time_str], ?FS),
            update_to_file(Three_speed_used_time_of_month_filepath, New_content);
        {error, Reason} ->
            {error, Reason}
    end.

get_new_used_ele_content(Filepath, Year_and_month_str, Used_time_str) ->
    case file:read_file(Filepath) of
        {ok, Binary} ->
            DataList = string:tokens(binary_to_list(Binary), ?NL),
            List1 = [DataLine || DataLine <- DataList, (not lists:prefix(Year_and_month_str, DataLine))],
            New_line = string:join([Year_and_month_str, Used_time_str], ?FS),
            List2 = lists:reverse([New_line | lists:reverse(List1)]),
            {ok, string:join(List2, ?NL)};
        {error, Reason} ->
            {error, Reason}
    end.

update_to_file(Filepath, New_content) ->
    filelib:ensure_dir(Filepath),
    case file:open(Filepath, [binary, write]) of
        {ok, Fd} ->
            io:fwrite(Fd, "~s~n", [New_content]),
            file:close(Fd),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.





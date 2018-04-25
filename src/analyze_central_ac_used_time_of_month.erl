-module (analyze_central_ac_used_time_of_month).

-include("analyze.hrl").
-include("analyze_config.hrl").
-include("analyze_meter.hrl").
-include("print.hrl").

-export([start/0, start/1]).

start() ->
    start(?HELP:date()).

start(Date) ->
    {Year, Month} = ?HELP:get_prev_month(Date),
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
            Last_used_time = analyze_report_util:get_used_time_sum(Meter_type, Last_line),
            {ok, Last_line_prev_month} = analyze_report_util:get_last_line(Last_filepath_prev_month),
            Last_used_time_prev_month = analyze_report_util:get_used_time_sum(Meter_type, Last_line_prev_month),
            cal_used_time_of_month(Meter, Year, Month, Last_used_time, Last_used_time_prev_month),
            ok;
        {ok, {2, Frist_filepath, Last_filepath}} ->
            {ok, Last_line} = analyze_report_util:get_last_line(Last_filepath),
            Last_used_time = analyze_report_util:get_used_time_sum(Meter_type, Last_line),
            {ok, Frist_line} = analyze_report_util:get_frist_line(Frist_filepath),
            Frist_used_time = analyze_report_util:get_used_time_sum(Meter_type, Frist_line),
            cal_used_time_of_month(Meter, Year, Month, Last_used_time, Frist_used_time),
            ok;
        {error, Reason} -> 
            ?ERROR("MODULE:~p get_meter_first_and_last_report_filepath_of_month(~p, ~p, ~p, ~p) is error: ~p", [?MODULE, Meter_type, Meter, Year, Month, Reason])
    end.

cal_used_time_of_month(Meter, Year, Month, Last_used_time, Frist_used_time) ->
    Used_time_str = ?HELP:float_to_decimal_str((Last_used_time-Frist_used_time)/60, 2),
    Year_and_month_str = ?HELP:year_and_month_str(Year, Month),
    New_content = string:join([Year_and_month_str, Used_time_str], ?FS),
    Used_time_of_month_filepath = analyze_util:get_used_time_of_month_filepath(Year, Meter),
    case append_to_file(Used_time_of_month_filepath, New_content) of
        ok -> ok;
        {error, Reason} ->
            ?ERROR("append_to_file is error:~p", [Reason])
    end,
    ok.

append_to_file(Filepath, New_content) ->
    filelib:ensure_dir(Filepath),
    case file:open(Filepath, [binary, append]) of
        {ok, Fd} ->
            io:fwrite(Fd, "~s~n", [New_content]),
            file:close(Fd),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.




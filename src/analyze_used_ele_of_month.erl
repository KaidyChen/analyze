-module (analyze_used_ele_of_month).

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
    [Do_fun(Meter_type) || Meter_type <- [?AC_TYPE, ?SOCKET_TYPE, ?FOUR_WAY_SWITCH_TYPE, ?CENTRAL_AC_TYPE]],
    ok.

do_work(Meter_type, Meter, Year, Month) ->
    case analyze_report_util:get_meter_first_and_last_report_filepath_of_month(Meter, Year, Month) of
        {ok, {1, Last_filepath_prev_month, Last_filepath}} ->
            {ok, Last_line} = analyze_report_util:get_last_line(Last_filepath),
            Last_ele = analyze_report_util:get_ele(Meter_type, Last_line),
            {ok, Last_line_prev_month} = analyze_report_util:get_last_line(Last_filepath_prev_month),
            Last_ele_prev_month = analyze_report_util:get_ele(Meter_type, Last_line_prev_month),
            cal_used_ele_of_month(Meter, Year, Month, Last_ele, Last_ele_prev_month),
            ok;
        {ok, {2, Frist_filepath, Last_filepath}} ->
            {ok, Last_line} = analyze_report_util:get_last_line(Last_filepath),
            Last_ele = analyze_report_util:get_ele(Meter_type, Last_line),
            {ok, Frist_line} = analyze_report_util:get_frist_line(Frist_filepath),
            Frist_ele = analyze_report_util:get_ele(Meter_type, Frist_line),
            cal_used_ele_of_month(Meter, Year, Month, Last_ele, Frist_ele),
            ok;
        {error, Reason} -> 
            ?PRINT("get_meter_first_and_last_report_filepath_of_month(~p, ~p, ~p, ~p) is error: ~p", [Meter_type, Meter, Year, Month, Reason])
    end.

cal_used_ele_of_month(Meter, Year, Month, Last_ele, Frist_ele) ->
    Used_ele_of_month_filepath = analyze_util:get_used_ele_of_month_filepath(Year, Meter),
    filelib:ensure_dir(Used_ele_of_month_filepath),
    Used_ele_str = ?HELP:float_to_decimal_str((Last_ele-Frist_ele), 2),
    Year_and_month_str = ?HELP:year_and_month_str(Year, Month),
    New_content = string:join([Year_and_month_str, Used_ele_str], ?FS),
    case append_to_file(Used_ele_of_month_filepath, New_content) of
        ok -> ok;
        {error, Reason} ->
            ?ERROR("append_to_file ~p is error:~p~n", [Used_ele_of_month_filepath, Reason])
    end,
    ok.

append_to_file(Filepath, New_content) ->
    case file:open(Filepath, [binary, append]) of
        {ok, Fd} ->
            io:fwrite(Fd, "~s~n", [New_content]),
            file:close(Fd),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.


-module (analyze_central_ac_used_time_of_day).

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
    [Do_fun(Meter_type) || Meter_type <- [?CENTRAL_AC_TYPE]],

    ok.

do_work(Meter_type, Meter, Date) ->
    case get_meter_first_and_last_report_used_time_of_date(Meter_type, Meter, Date) of
        {ok, {First_used_time_1, Last_used_time_1}} ->
            case get_meter_first_and_last_report_used_time_of_date(Meter_type, Meter, ?HELP:addDay(Date, -1)) of
                {ok, {First_used_time_2, Last_used_time_2}} ->
                    cal_used_time_of_day(Meter, Date, Last_used_time_1, Last_used_time_2);
                {error, What} ->
                    cal_used_time_of_day(Meter, Date, Last_used_time_1, First_used_time_1)
            end;
        {error, Reason} ->
            ?ERROR("MODULE:~p get_meter_first_and_last_report_used_time_of_date(~p, ~p, ~p) is error: ~p", [?MODULE, Meter_type, Meter, Date, Reason])
    end.

get_meter_first_and_last_report_used_time_of_date(Meter_type, Meter, Date) ->
    case get_report_list(Meter, Date) of
        {ok, Report_list} ->
            [First_line | _] = Report_list,
            [Last_line | _] = lists:reverse(Report_list),
            First_used_time = analyze_report_util:get_used_time_sum(Meter_type, First_line),
            Last_used_time = analyze_report_util:get_used_time_sum(Meter_type, Last_line),
            {ok, {First_used_time, Last_used_time}};
        {error, Reason} -> 
            {error, Reason}
    end.

get_report_list(Meter, Date) ->
    Meter_datamsg_filepath = analyze_report_util:get_meter_datamsg_filepath(Meter, Date),
    case file:read_file(Meter_datamsg_filepath) of
        {ok, Data_bin} ->
            Data_list = binary_to_list(Data_bin),
            {ok, string:tokens(Data_list, ?NL)};
        {error, Reason} -> {error, Reason}
    end.

cal_used_time_of_day(Meter, Date, Last_used_time, First_used_time) ->
    Used_time_str = ?HELP:float_to_decimal_str((Last_used_time-First_used_time)/60, 2),
    Date_str = ?HELP:dateToStr(Date, "-"),
    New_content = string:join([Date_str, Used_time_str], ?FS),
    Used_time_of_day_filepath = analyze_util:get_used_time_of_day_filepath(Date, Meter),
    case append_to_file(Used_time_of_day_filepath, New_content) of
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

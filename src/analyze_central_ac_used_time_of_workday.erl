-module (analyze_central_ac_used_time_of_workday).

-include("analyze.hrl").
-include("analyze_config.hrl").
-include("analyze_meter.hrl").
-include("print.hrl").

-export([start/0, start/1]).

start() ->
    start(?HELP:date()).

start(Today) ->
    Date = ?HELP:addDay(Today, -1), 
    case analyze_util:is_holiday(Date) of
        false ->
            Do_fun = fun
                (Meter_type) ->
                    All_XX_meter = analyze_meter_util:select_meter_by_meter_type(Meter_type),
                    lists:foreach(fun
                        ([Meter]) ->
                            do_work(Meter_type, Meter, Date)
                    end, All_XX_meter)
            end,
            [Do_fun(Meter_type) || Meter_type <- [?CENTRAL_AC_TYPE]];
        true ->
            ok
    end,
    ok.

do_work(Meter_type, Meter, Date) ->
    case get_report_list(Meter, Date) of
        {ok, []} -> ok;
        {ok, Report_list} ->
            [First_line | _] = Report_list,
            [Last_line | _] = lists:reverse(Report_list),
            First_used_time = analyze_report_util:get_used_time_sum(Meter_type, First_line),
            Last_used_time = analyze_report_util:get_used_time_sum(Meter_type, Last_line),
            Worktime_used_time = cal_worktime_used_time(Meter_type, Report_list),
            Nonworktime_used_time = (Last_used_time - First_used_time) - Worktime_used_time,
            Nonworktime_used_time_tmp = case (Nonworktime_used_time > 0) of
                true ->
                    Nonworktime_used_time;
                false ->
                    0
            end,
            ?PRINT("~p ~p~n", [Worktime_used_time, Nonworktime_used_time_tmp]),
            save_workday_used_time(Date, Meter, Worktime_used_time, Nonworktime_used_time_tmp),
            ok;
        {error, Reason} ->
            ?ERROR("MODULE:~p get_meter_first_and_last_report_ele_of_date(~p, ~p, ~p) is error: ~p", [?MODULE, Meter_type, Meter, Date, Reason])
    end.

get_report_list(Meter, Date) ->
    Meter_datamsg_filepath = analyze_report_util:get_meter_datamsg_filepath(Meter, Date),
    case file:read_file(Meter_datamsg_filepath) of
        {ok, Data_bin} ->
            Data_list = binary_to_list(Data_bin),
            {ok, string:tokens(Data_list, ?NL)};
        {error, Reason} -> {error, Reason}
    end.

cal_worktime_used_time(Meter_type, Report_list) ->
    Fun = fun
        (Data_line) ->
            {Date, Time} = analyze_report_util:get_datetime_tuple(Meter_type, Data_line),
            case {?WORKTIME_STERT =< Time, Time =< ?WORKTIME_END} of
                {true, true} ->
                    Used_time = analyze_report_util:get_used_time_sum(Meter_type, Data_line),
                    {true, Used_time};
                _ ->
                    false
            end
    end,
    Worktime_used_time_list = lists:filtermap(Fun, lists:sort(Report_list)),
    case Worktime_used_time_list of
        [] -> 0;
        _ ->
            [First_worktime_used_time | _] = Worktime_used_time_list,
            [Last_worktime_used_time | _] = lists:reverse(Worktime_used_time_list),  
            (Last_worktime_used_time - First_worktime_used_time)
    end.

save_workday_used_time(Date, Meter, Worktime_used_time, Nonworktime_used_time) ->
    Worktime_used_time_str = ?HELP:float_to_decimal_str(Worktime_used_time/60, 2),
    Nonworktime_used_time_str = ?HELP:float_to_decimal_str(Nonworktime_used_time/60, 2),
    Date_str = ?HELP:dateToStr(Date, "-"),
    New_content = string:join([Date_str, Worktime_used_time_str, Nonworktime_used_time_str], ?FS),
    Used_time_of_workday_filepath = analyze_util:get_used_time_of_workday_filepath(Date, Meter),
    ?PRINT("~p~n", [New_content]),
    filelib:ensure_dir(Used_time_of_workday_filepath),
    case append_to_file(Used_time_of_workday_filepath, New_content) of
        ok -> ok;
        {error, Reason} ->
            ?ERROR("append_to_file is error:~p", [Reason])
    end.

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

-module (analyze_used_ele_of_workday).

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
            [Do_fun(Meter_type) || Meter_type <- [?AC_TYPE, ?SOCKET_TYPE, ?FOUR_WAY_SWITCH_TYPE, ?CENTRAL_AC_TYPE]];
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
            First_ele = analyze_report_util:get_ele(Meter_type, First_line),
            Last_ele = analyze_report_util:get_ele(Meter_type, Last_line),
            Worktime_used_ele = cal_worktime_used_ele(Meter_type, Report_list),
            ?PRINT("~p ~p ~p~n", [Last_ele, First_ele, Worktime_used_ele]),
            Nonworktime_used_ele = Last_ele - First_ele - Worktime_used_ele,
            Nonworktime_used_ele_tmp = case (Nonworktime_used_ele > 0) of
                true ->
                    Nonworktime_used_ele;
                false ->
                    0.0
            end,
            save_workday_used_ele(Date, Meter, Worktime_used_ele, Nonworktime_used_ele_tmp),
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

cal_worktime_used_ele(Meter_type, Report_list) ->
    Fun = fun
        (Data_line) ->
            {Date, Time} = analyze_report_util:get_datetime_tuple(Meter_type, Data_line),
            Ele = analyze_report_util:get_ele(Meter_type, Data_line),
            case {?WORKTIME_STERT =< Time, Time =< ?WORKTIME_END} of
                {true, true} ->
                    {true, Ele};
                _ ->
                    false
            end
    end,
    Worktime_used_ele_list = lists:filtermap(Fun, lists:sort(Report_list)),
    case Worktime_used_ele_list of
        [] -> 0.0;
        _ ->
            [First_worktime_used_ele | _] = Worktime_used_ele_list,
            [Last_worktime_used_ele | _] = lists:reverse(Worktime_used_ele_list),  
            ?HELP:floatDecimal(Last_worktime_used_ele - First_worktime_used_ele, 2)
    end.

save_workday_used_ele(Date, Meter, Worktime_used_ele, Nonworktime_used_ele) ->
    Worktime_used_ele_str = ?HELP:float_to_decimal_str(Worktime_used_ele, 2),
    Nonworktime_used_ele_str = ?HELP:float_to_decimal_str(Nonworktime_used_ele, 2),
    Date_str = ?HELP:dateToStr(Date, "-"),
    New_content = string:join([Date_str, Worktime_used_ele_str, Nonworktime_used_ele_str], ?FS),
    Used_ele_of_workday_filepath = analyze_util:get_used_ele_of_workday_filepath(Date, Meter),
    ?PRINT("~p~n", [New_content]),
    filelib:ensure_dir(Used_ele_of_workday_filepath),
    case append_to_file(Used_ele_of_workday_filepath, New_content) of
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

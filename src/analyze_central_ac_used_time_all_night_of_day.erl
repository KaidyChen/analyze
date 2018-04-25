-module (analyze_central_ac_used_time_all_night_of_day).

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
    case get_report_list(Meter, Date) of
        {ok, Report_list} ->
            All_night_used_time = cal_all_night_used_time(Meter_type, Report_list),
            All_night_used_time_str = ?HELP:float_to_decimal_str(All_night_used_time/60, 2),
            Date_str = ?HELP:dateToStr(Date, "-"),
            New_content = string:join([Date_str, All_night_used_time_str], ?FS),
            Used_time_all_night_of_day_filepath = analyze_util:get_used_time_all_night_of_day_filepath(Date, Meter),
            case append_to_file(Used_time_all_night_of_day_filepath, New_content) of
                ok -> ok;
                {error, Reason} ->
                    ?ERROR("append_to_file is error:~p", [Reason])
            end,
            ok;
        {error, Reason} ->
            ?ERROR("get_report_list(~p, ~p) is error:~p~n", [Meter, Date, Reason]),
            ok
    end.
    
get_report_list(Meter, Date) ->
    Meter_datamsg_filepath = analyze_report_util:get_meter_datamsg_filepath(Meter, Date),
    case file:read_file(Meter_datamsg_filepath) of
        {ok, Data_bin} ->
            Data_list = binary_to_list(Data_bin),
            {ok, string:tokens(Data_list, ?NL)};
        {error, Reason} -> {error, Reason}
    end.

cal_all_night_used_time(Meter_type, Report_list) ->
    Fun = fun
        (Report_line, {Used_time_list_1_tmp, Used_time_list_2_tmp}) ->
            {_Date, Time} = analyze_report_util:get_datetime_tuple(Meter_type, Report_line),
            Used_time = analyze_report_util:get_used_time_sum(Meter_type, Report_line),
            case {Time > {23, 0, 0}, Time < {6, 0, 0}} of
                {true, _} ->
                    {[Used_time | Used_time_list_1_tmp], Used_time_list_2_tmp};
                {_, true} ->
                    {Used_time_list_1_tmp, [Used_time | Used_time_list_2_tmp]};
                _ ->
                    {Used_time_list_1_tmp, Used_time_list_2_tmp}
            end
    end,
    {Used_time_list_1, Used_time_list_2} = lists:foldl(Fun, {[], []}, lists:sort(Report_list)),
    Used_time_1 = cal_used_time_diff(Used_time_list_1),
    Used_time_2 = cal_used_time_diff(Used_time_list_2),
    Used_time_1+Used_time_2.

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

cal_used_time_diff(Used_time_list) when (Used_time_list =/= []) ->
    lists:max(Used_time_list) - lists:min(Used_time_list);
cal_used_time_diff(Used_time_list) ->
    0.




-module (analyze_used_time_of_workday_month).

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
    Used_time_of_workday_filepath = analyze_util:get_used_time_of_workday_filepath(Year, Month, Meter),
    case get_workday_used_time_line_list(Used_time_of_workday_filepath) of
        {ok, []} -> ok;
        {ok, Data_line_list} ->
            Fun = fun
                (Data_line, {Worktime_used_time_list_tmp, Nonworktime_used_time_list_tmp}) ->
                    case string:tokens(Data_line, ?FS) of
                        [Date_str, Worktime_used_time_str, Nonworktime_used_time_str] ->
                            Worktime_used_ele_list_tmp_new = [list_to_float(Worktime_used_time_str) | Worktime_used_time_list_tmp],
                            Nonworktime_used_ele_list_tmp_new = [list_to_float(Nonworktime_used_time_str) | Nonworktime_used_time_list_tmp],
                            {Worktime_used_ele_list_tmp_new, Nonworktime_used_ele_list_tmp_new};
                        _ ->
                            {Worktime_used_time_list_tmp, Nonworktime_used_time_list_tmp}
                    end
            end,
            {Worktime_used_time_list, Nonworktime_used_time_list} = lists:foldl(Fun, {[], []}, Data_line_list),
            Worktime_used_time_sum = get_sum(Worktime_used_time_list),
            Nonworktime_used_time_sum = get_sum(Nonworktime_used_time_list),
            save_workday_month_used_time(Year, Month, Meter, Worktime_used_time_sum, Nonworktime_used_time_sum),
            ?PRINT("~p ~p ~p~n", [Meter, Worktime_used_time_sum, Nonworktime_used_time_sum]),
            ok;
        {error, Reason} ->
            ?ERROR("get_workday_used_time_line_list(~p) is error: ~p", [Used_time_of_workday_filepath, Reason])
    end,
    ok.

get_workday_used_time_line_list(Used_time_of_workday_filepath) ->
    case file:read_file(Used_time_of_workday_filepath) of
        {ok, Data_bin} ->
            Data_list = binary_to_list(Data_bin),
            Data_line_list = string:tokens(Data_list, ?NL),
            {ok, lists:usort(Data_line_list)};
        {error, Reason} ->
            {error, Reason}
    end.

get_sum(List) when  ([] =:= List) ->
    0.0;
get_sum(List) ->
    lists:sum(List).

save_workday_month_used_time(Year, Month, Meter, Worktime_used_time_sum, Nonworktime_used_time_sum) ->
    Worktime_used_time_sum_str = ?HELP:float_to_decimal_str(Worktime_used_time_sum, 2),
    Nonworktime_used_time_sum_str = ?HELP:float_to_decimal_str(Nonworktime_used_time_sum, 2),
    Year_month_str = ?HELP:year_and_month_str(Year, Month),
    New_content = string:join([Year_month_str, Worktime_used_time_sum_str, Nonworktime_used_time_sum_str], ?FS),
    Used_time_of_workday_month_filepath = analyze_util:get_used_time_of_workday_month_filepath(Year, Meter),
    ?PRINT("~p~n", [New_content]),
    case append_to_file(Used_time_of_workday_month_filepath, New_content) of
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

-module (analyze_used_ele_of_month_work_and_nonwork).

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
    case get_workday_and_nonworkday_used_ele_list(Meter_type, Meter, Year, Month) of
        {ok, Workday_used_ele_list, Nonworkday_used_ele_list} ->
            % ?PRINT("Workday_used_ele_list:~p~n", [Workday_used_ele_list]),
            Workday_used_ele_daynum = length(Workday_used_ele_list),
            Workday_used_ele_sum = get_used_ele_sum(Workday_used_ele_list),
            Nonworkday_used_ele_daynum = length(Nonworkday_used_ele_list),
            Nonworkday_used_ele_sum = get_used_ele_sum(Nonworkday_used_ele_list),
            % ?PRINT("Workday_used_ele_daynum:~p Nonworkday_used_ele_daynum:~p~n", [Workday_used_ele_daynum, Nonworkday_used_ele_daynum]),
            ?PRINT("Workday_used_ele_sum:~p Nonworkday_used_ele_sum:~p~n", [Workday_used_ele_sum, Nonworkday_used_ele_sum]),
            save_workday_and_nonworkday_info(Meter_type, Meter, Year, Month, Workday_used_ele_daynum, Workday_used_ele_sum, Nonworkday_used_ele_daynum, Nonworkday_used_ele_sum),
            ok;
        {error, Reason} ->
            ?ERROR("get_workday_and_nonworkday_used_ele_list(~p, ~p, ~p, ~p) is error:~p", [Meter_type, Meter, Year, Month, Reason])
    end,
    ok.

get_workday_and_nonworkday_used_ele_list(Meter_type, Meter, Year, Month) ->
    case analyze_util:get_meter_used_ele_list_of_day(Meter, Year, Month) of
        {ok, Data_line_list} ->
            Fun = fun
                (Data_line, {Workday_used_ele_list_tmp, Nonworkday_used_ele_list_tmp}) ->
                    case string:tokens(Data_line, ?FS) of
                        [Date_str, Used_ele_str] ->
                            case analyze_util:is_holiday(Date_str) of
                                false ->
                                    {[list_to_float(Used_ele_str) | Workday_used_ele_list_tmp], Nonworkday_used_ele_list_tmp};
                                true ->
                                    {Workday_used_ele_list_tmp, [list_to_float(Used_ele_str) |Nonworkday_used_ele_list_tmp]}
                            end;
                        _ ->
                            {Workday_used_ele_list_tmp, Nonworkday_used_ele_list_tmp}
                    end
            end,
            {Workday_used_ele_list, Nonworkday_used_ele_list} = lists:foldl(Fun, {[], []}, Data_line_list),
            {ok, Workday_used_ele_list, Nonworkday_used_ele_list};
        {error, Reason} ->
            {error, Reason}
    end.

get_used_ele_sum(Used_ele_list) when is_list(Used_ele_list) ->
    case Used_ele_list of
        [] -> 0.0;
        _ ->
            ?HELP:floatDecimal(lists:sum(Used_ele_list), 2)
    end.

save_workday_and_nonworkday_info(Meter_type, Meter, Year, Month, Workday_used_ele_daynum, Workday_used_ele_sum, Nonworkday_used_ele_daynum, Nonworkday_used_ele_sum)->
    Work_and_nonwork_filepath = analyze_util:get_used_ele_of_month_work_and_nonwork_filepath(Year, Meter),
    Year_month_str = ?HELP:year_and_month_str(Year, Month),
    Workday_used_ele_daynum_str = integer_to_list(Workday_used_ele_daynum),
    Workday_used_ele_sum_str = ?HELP:float_to_decimal_str(Workday_used_ele_sum, 2),
    Nonworkday_used_ele_daynum_str = integer_to_list(Nonworkday_used_ele_daynum),
    Nonworkday_used_ele_sum_str = ?HELP:float_to_decimal_str(Nonworkday_used_ele_sum, 2),
    New_content = string:join([Year_month_str, Workday_used_ele_daynum_str, Workday_used_ele_sum_str, Nonworkday_used_ele_daynum_str, Nonworkday_used_ele_sum_str], ?FS),
    % ?PRINT("~s~n", [Work_and_nonwork_filepath]),
    % ?PRINT("~s~n", [New_content]),
    filelib:ensure_dir(Work_and_nonwork_filepath),
    case append_to_file(Work_and_nonwork_filepath, New_content) of
        ok -> ok;
        {error, Reason} ->
            ?ERROR("MODULE:~p append_to_file is error:~p", [?MODULE, Reason])
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





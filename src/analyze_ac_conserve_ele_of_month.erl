-module (analyze_ac_conserve_ele_of_month).

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
    [Do_fun(Meter_type) || Meter_type <- [?AC_TYPE]],
    ok.

do_work(Meter_type, Meter, Year, Month) ->
    Every_day_conserve_ele_of_day_filepath = analyze_util:get_conserve_ele_of_day_filepath(Year, Month, Meter),
    case get_conserve_ele_line_list(Every_day_conserve_ele_of_day_filepath) of
        {ok, Data_line_list} ->
            Fun = fun
                (Data_line) ->
                    case string:tokens(Data_line, ?FS) of
                        [Date_str, Conserve_ele_str] ->
                            {true, list_to_float(Conserve_ele_str)};
                        _ ->
                            false
                    end
            end,
            Conserve_ele_list = lists:filtermap(Fun, Data_line_list),
            Conserve_ele_of_month_str = ?HELP:float_to_decimal_str(lists:sum(Conserve_ele_list), 2),
            Year_month_str = ?HELP:year_and_month_str(Year, Month),
            New_content = string:join([Year_month_str, Conserve_ele_of_month_str], ?FS),
            Conserve_ele_of_month_filepath = analyze_util:get_conserve_ele_of_month_filepath(Year, Meter),
            filelib:ensure_dir(Conserve_ele_of_month_filepath),
            case append_to_file(Conserve_ele_of_month_filepath, New_content) of
                ok -> ok;
                {error, Reason} ->
                    ?ERROR("append_to_file is error:~p", [Reason])
            end,
            ok;
        {error, Reason} ->
            ?ERROR("get_used_time_list(~p) is error: ~p", [Every_day_conserve_ele_of_day_filepath, Reason])
    end.

get_conserve_ele_line_list(Every_day_conserve_ele_of_day_filepath) ->
    case file:read_file(Every_day_conserve_ele_of_day_filepath) of
        {ok, Data_bin} ->
            Data_list = binary_to_list(Data_bin),
            Data_line_list = string:tokens(Data_list, ?NL),
            {ok, lists:usort(Data_line_list)};
        {error, Reason} ->
            {error, Reason}
    end.

append_to_file(Filepath, New_content) ->
    case file:open(Filepath, [binary, append]) of
        {ok, Fd} ->
            io:fwrite(Fd, "~s~n", [New_content]),
            file:close(Fd),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

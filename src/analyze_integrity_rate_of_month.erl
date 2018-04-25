-module (analyze_integrity_rate_of_month).

-include("analyze.hrl").
-include("analyze_config.hrl").
-include("analyze_meter.hrl").
-include("print.hrl").

-export([start/0, start/1]).

start() ->
    start(?HELP:date()).

start(Date) ->
    {Year, Month} = ?HELP:get_prev_month(Date),
    All_meter_type_and_meter = analyze_meter_util:get_all_meter_type_and_meter(),
    lists:foreach(fun
        ([{Meter_type, Meter}]) ->
            do_work(Meter_type, Meter, Year, Month)
    end, All_meter_type_and_meter),
    ok.

do_work(Meter_type, Meter, Year, Month) ->
    Every_day_integrity_rate_of_day_filepath = analyze_util:get_integrity_rate_of_day_filepath(Year, Month, Meter),
    case get_integrity_rate_list(Every_day_integrity_rate_of_day_filepath) of
        {ok, []} -> ok;
        {ok, Integrity_rate_list} ->
            Integrity_rate_of_month = lists:sum(Integrity_rate_list) / length(Integrity_rate_list),
            Year_month_str = ?HELP:year_and_month_str(Year, Month),
            Integrity_rate_of_month_str = ?HELP:float_to_decimal_str(Integrity_rate_of_month, 2),
            New_content = string:join([Year_month_str, Integrity_rate_of_month_str], ?FS),
            Integrity_rate_of_month_filepath = analyze_util:get_integrity_rate_of_month_filepath(Year, Meter),
            filelib:ensure_dir(Integrity_rate_of_month_filepath),
            case append_to_file(Integrity_rate_of_month_filepath, New_content) of
                ok -> ok;
                {error, Reason} ->
                    ?ERROR("append_to_file is error:~p", [Reason])
            end,
            ok;
        {error, Reason} ->
            ?ERROR("get_integrity_rate_list(~p) is error: ~p", [Every_day_integrity_rate_of_day_filepath, Reason])
    end.


get_integrity_rate_list(Integrity_rate_of_month_filepath) ->
    case file:read_file(Integrity_rate_of_month_filepath) of
        {ok, Data_bin} ->
            Data_list = binary_to_list(Data_bin),
            Data_line_list = string:tokens(Data_list, ?NL),
            Fun = fun
                (Data_line) ->
                    case string:tokens(Data_line, ?FS) of
                        [Date_str, Integrity_rate_str] ->
                            {true, list_to_float(Integrity_rate_str)};
                        _ ->
                            false
                    end
            end,
            {ok, lists:filtermap(Fun, lists:usort(Data_line_list))};
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

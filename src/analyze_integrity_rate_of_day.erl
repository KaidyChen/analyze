-module (analyze_integrity_rate_of_day).

-include("analyze.hrl").
-include("analyze_config.hrl").
-include("analyze_meter.hrl").
-include("print.hrl").

-export([start/0, start/1]).

start() ->
    start(?HELP:date()).

start(Today) ->
    Date = ?HELP:addDay(Today, -1), 
    All_meter_type_and_meter = analyze_meter_util:get_all_meter_type_and_meter(),
    lists:foreach(fun
        ([{Meter_type, Meter}]) ->
            do_work(Meter_type, Meter, Date)
    end, All_meter_type_and_meter),

    ok.

do_work(Meter_type, Meter, Date) ->
    case get_integrity_rate_list_of_day(Meter, Date) of
        {ok, []} -> ok;
        {ok, Integrity_rate_list} ->
            Avg_integrity_rate = lists:sum(Integrity_rate_list) / length(Integrity_rate_list),
            ?PRINT("Avg_integrity_rate:~p~n", [Avg_integrity_rate]),
            save_integrity_rate_of_day(Date, Meter, Avg_integrity_rate),
            ok;
        {error, Reason} ->
            ?ERROR("get_integrity_rate_list_of_day is error: ~p", [Reason])    
    end,
    ok.

get_integrity_rate_list_of_day(Meter, Date) ->
    {Year, Month, _Day} = Date,
    Filepath = analyze_util:get_integrity_rate_filepath(Year, Month, Meter),
    case file:read_file(Filepath) of
        {ok, Data_bin} ->
            Data_list = binary_to_list(Data_bin),
            Data_line_list = string:tokens(Data_list, ?NL),
            Date_str = ?HELP:dateToStr(Date, "-"),
            Fun = fun
                (Data_line) ->
                    case string:tokens(Data_line, ?FS) of
                        [Date_str, _, _, _, Integrity_rate_str] ->
                            {true, list_to_float(Integrity_rate_str)};
                        _ ->
                            false    
                    end
            end,
            {ok, lists:filtermap(Fun, Data_line_list)};
        {error, Reason} ->
            {error, Reason}     
    end.

save_integrity_rate_of_day(Date, Meter, Avg_integrity_rate) ->
    Avg_integrity_rate_str = ?HELP:float_to_decimal_str(Avg_integrity_rate, 2),
    Date_str = ?HELP:dateToStr(Date, "-"),
    New_content = string:join([Date_str, Avg_integrity_rate_str], ?FS),
    Integrity_rate_of_day_filepath = analyze_util:get_integrity_rate_of_day_filepath(Date, Meter),
    filelib:ensure_dir(Integrity_rate_of_day_filepath),
    case append_to_file(Integrity_rate_of_day_filepath, New_content) of
        ok -> ok;
        {error, Reason} ->
            ?ERROR("append_to_file is error:~p", [Reason])
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

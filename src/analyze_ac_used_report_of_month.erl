-module (analyze_ac_used_report_of_month).

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
    New_content = get_used_report_of_month(Meter_type, Meter, Year, Month),
    ?PRINT("~p~n", [New_content]),
    Used_report_of_month_filepath = analyze_util:get_used_report_of_month_filepath(Year, Meter),
    case append_to_file(Used_report_of_month_filepath, New_content) of
        ok -> ok;
        {error, Reason} ->
            ?ERROR("append_to_file is error:~p", [Reason])
    end,
    ok.

get_used_report_of_month(Meter_type, Meter, Year, Month) ->
    Year_and_month_str = ?HELP:year_and_month_str(Year, Month),
    Used_ele_of_month = get_meter_used_ele_of_month(Meter_type, Meter, Year, Month),
    Conserve_ele_of_month = get_meter_conserve_ele_of_month(Meter_type, Meter, Year, Month),
    Used_time_of_month = get_used_time_of_month(Meter_type, Meter, Year, Month),
    {Ranking, Ranking_beat} = get_ranging_info(Meter_type, Meter, Year, Month),
    {Workday_used_ele_sum_str, Nonworkday_used_ele_sum_str} = get_used_ele_of_month_work_and_nonwork(Meter_type, Meter, Year, Month),
    {Worktime_used_ele_sum_str, Nonworktime_used_ele_sum_str} = get_used_ele_of_workday_month(Meter_type, Meter, Year, Month),
    Integrity_rate_of_month_str = get_integrity_rate_of_month(Meter_type, Meter, Year, Month),
    Commu_rate_of_month_str = get_commu_rate_of_month(Meter_type, Meter, Year, Month),

    New_content = string:join([Year_and_month_str, Used_ele_of_month, Conserve_ele_of_month, Used_time_of_month, Ranking, Ranking_beat, Workday_used_ele_sum_str, Worktime_used_ele_sum_str, Nonworktime_used_ele_sum_str, Nonworkday_used_ele_sum_str, Integrity_rate_of_month_str, Commu_rate_of_month_str], ?FS),
    New_content.
    

get_meter_used_ele_of_month(Meter_type, Meter, Year, Month) ->
    Filepath = analyze_util:get_used_ele_of_month_filepath(Year, Meter),
    case file:read_file(Filepath) of
        {ok, Data_bin} ->
            Data_bitstring = binary_to_list(Data_bin),
            Data_line_list = string:tokens(Data_bitstring, ?NL),
            Year_and_month_str = ?HELP:year_and_month_str(Year, Month),
            get_meter_used_ele_of_month_(Year_and_month_str, Data_line_list);
        {error, Reason} -> ?NONE_DATA
    end.  

get_meter_used_ele_of_month_(Year_and_month_str, [Data_line | T]) ->
    case string:tokens(Data_line, ?FS) of
        [Year_and_month_str, Used_ele_str] ->
            Used_ele_str;
        _ ->
            get_meter_used_ele_of_month_(Year_and_month_str, T)
    end;
get_meter_used_ele_of_month_(Year_and_month_str, []) ->
    ?NONE_DATA.

get_meter_conserve_ele_of_month(Meter_type, Meter, Year, Month) ->
    Filepath = analyze_util:get_conserve_ele_of_month_filepath(Year, Meter),
    case file:read_file(Filepath) of
        {ok, Data_bin} ->
            Data_bitstring = binary_to_list(Data_bin),
            Data_line_list = string:tokens(Data_bitstring, ?NL),
            Year_and_month_str = ?HELP:year_and_month_str(Year, Month),
            get_meter_conserve_ele_of_month_(Year_and_month_str, Data_line_list);
        {error, Reason} -> ?NONE_DATA
    end.  

get_meter_conserve_ele_of_month_(Year_and_month_str, [Data_line | T]) ->
    case string:tokens(Data_line, ?FS) of
        [Year_and_month_str, Conserve_ele_str] ->
            Conserve_ele_str;
        _ ->
            get_meter_conserve_ele_of_month_(Year_and_month_str, T)
    end;
get_meter_conserve_ele_of_month_(Year_and_month_str, []) ->
    ?NONE_DATA.

get_used_time_of_month(Meter_type, Meter, Year, Month) ->
    Filepath = analyze_util:get_used_time_of_month_filepath(Year, Meter),
    case file:read_file(Filepath) of
        {ok, Data_bin} ->
            Data_bitstring = binary_to_list(Data_bin),
            Data_line_list = string:tokens(Data_bitstring, ?NL),
            Year_and_month_str = ?HELP:year_and_month_str(Year, Month),
            get_used_time_of_month_(Year_and_month_str, Data_line_list);
        {error, Reason} -> ?NONE_DATA
    end.  

get_used_time_of_month_(Year_and_month_str, [Data_line | T]) ->
    case string:tokens(Data_line, ?FS) of
        [Year_and_month_str, Used_time_str] ->
            Used_time_str;
        _ ->
            get_used_time_of_month_(Year_and_month_str, T)
    end;
get_used_time_of_month_(Year_and_month_str, []) ->
    ?NONE_DATA.


get_ranging_info(Meter_type, Meter, Year, Month) ->
    {Ranking, Ranking_beat} = case analyze_util:get_ranking_list(Year, Month) of
        {ok, Data_line_list} ->
            Ranking_total_num_str = integer_to_list(length(Data_line_list)),
            get_ranging_info_(Meter, Data_line_list, Ranking_total_num_str);
        {error, Reason} -> 
            {?NONE_DATA, ?NONE_DATA}
    end,
    {Ranking, Ranking_beat}.

get_ranging_info_(Meter, [Data_line | T], Ranking_total_num_str) ->
    case string:tokens(Data_line, ?FS) of
        [Meter, Used_ele_day_num, Save_ele_day_num, Save_ratio, Ranking_tmp, Ranking_beat_tmp] ->
            Ranking = lists:concat([Ranking_tmp, "/", Ranking_total_num_str]),
            Ranking_beat = lists:concat([integer_to_list((trunc(list_to_float(Ranking_beat_tmp) * 100))), "%"]),
            {Ranking, Ranking_beat};
        _ ->
            get_ranging_info_(Meter, T, Ranking_total_num_str)
    end;
get_ranging_info_(Meter, [], Ranking_total_num_str) ->
    {?NONE_DATA, ?NONE_DATA}.


get_used_ele_of_month_work_and_nonwork(Meter_type, Meter, Year, Month) ->
    Filepath = analyze_util:get_used_ele_of_month_work_and_nonwork_filepath(Year, Meter),
    case file:read_file(Filepath) of
        {ok, Data_bin} ->
            Data_bitstring = binary_to_list(Data_bin),
            Data_line_list = string:tokens(Data_bitstring, ?NL),
            Year_and_month_str = ?HELP:year_and_month_str(Year, Month),
            get_used_ele_of_month_work_and_nonwork_(Year_and_month_str, Data_line_list);
        {error, Reason} -> 
            {?NONE_DATA, ?NONE_DATA}
    end.  

get_used_ele_of_month_work_and_nonwork_(Year_and_month_str, [Data_line | T]) ->
    case string:tokens(Data_line, ?FS) of
        [Year_and_month_str, _Workday_used_ele_daynum_str, Workday_used_ele_sum_str, _Nonworkday_used_ele_daynum_str, Nonworkday_used_ele_sum_str] ->
            {Workday_used_ele_sum_str, Nonworkday_used_ele_sum_str};
        _ ->
            get_used_ele_of_month_work_and_nonwork_(Year_and_month_str, T)
    end;
get_used_ele_of_month_work_and_nonwork_(Year_and_month_str, []) ->
    {?NONE_DATA, ?NONE_DATA}.


get_used_ele_of_workday_month(Meter_type, Meter, Year, Month) ->
    Filepath = analyze_util:get_used_ele_of_workday_month_filepath(Year, Meter),
    case file:read_file(Filepath) of
        {ok, Data_bin} ->
            Data_bitstring = binary_to_list(Data_bin),
            Data_line_list = string:tokens(Data_bitstring, ?NL),
            Year_and_month_str = ?HELP:year_and_month_str(Year, Month),
            get_used_ele_of_workday_month_(Year_and_month_str, Data_line_list);
        {error, Reason} -> 
            {?NONE_DATA, ?NONE_DATA}
    end.  

get_used_ele_of_workday_month_(Year_and_month_str, [Data_line | T]) ->
    case string:tokens(Data_line, ?FS) of
        [Year_and_month_str, Worktime_used_ele_sum_str, Nonworktime_used_ele_sum_str] ->
            {Worktime_used_ele_sum_str, Nonworktime_used_ele_sum_str};
        _ ->
            get_used_ele_of_workday_month_(Year_and_month_str, T)
    end;
get_used_ele_of_workday_month_(Year_and_month_str, []) ->
    {?NONE_DATA, ?NONE_DATA}.

get_integrity_rate_of_month(Meter_type, Meter, Year, Month) ->
    Filepath = analyze_util:get_integrity_rate_of_month_filepath(Year, Meter),
    case file:read_file(Filepath) of
        {ok, Data_bin} ->
            Data_bitstring = binary_to_list(Data_bin),
            Data_line_list = string:tokens(Data_bitstring, ?NL),
            Year_and_month_str = ?HELP:year_and_month_str(Year, Month),
            get_integrity_rate_of_month_(Year_and_month_str, Data_line_list);
        {error, Reason} -> ?NONE_DATA
    end.  

get_integrity_rate_of_month_(Year_and_month_str, [Data_line | T]) ->
    case string:tokens(Data_line, ?FS) of
        [Year_and_month_str, Integrity_rate_of_month_str] ->
            Integrity_rate_of_month_str;
        _ ->
            get_integrity_rate_of_month_(Year_and_month_str, T)
    end;
get_integrity_rate_of_month_(Year_and_month_str, []) ->
    ?NONE_DATA.

get_commu_rate_of_month(Meter_type, Meter, Year, Month) ->
    Filepath = analyze_util:get_commu_rate_of_month_filepath(Year, Meter),
    case file:read_file(Filepath) of
        {ok, Data_bin} ->
            Data_bitstring = binary_to_list(Data_bin),
            Data_line_list = string:tokens(Data_bitstring, ?NL),
            Year_and_month_str = ?HELP:year_and_month_str(Year, Month),
            Used_ele_str = get_commu_rate_of_month_(Year_and_month_str, Data_line_list),
            Used_ele_str;
        {error, Reason} -> ?NONE_DATA
    end.          

get_commu_rate_of_month_(Year_and_month_str, [Data_line | T]) ->
    case string:tokens(Data_line, ?FS) of
        [Year_and_month_str, Commu_rate_of_month_str] ->
            Commu_rate_of_month_str;
        _ ->
            get_commu_rate_of_month_(Year_and_month_str, T)
    end;
get_commu_rate_of_month_(Year_and_month_str, []) ->
    ?NONE_DATA.

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
    






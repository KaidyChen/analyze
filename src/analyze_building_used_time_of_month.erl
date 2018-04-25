-module (analyze_building_used_time_of_month).

-include("analyze.hrl").
-include("analyze_config.hrl").
-include("analyze_meter.hrl").
-include("print.hrl").

-export([start/0, start/1]).

start() ->
    start(?HELP:date()).

start(Date) ->
    {Year, Month} = ?HELP:get_prev_month(Date),
    cal_building_used_time_of_month_by_meter_type(?AC_TYPE, Year, Month),
    cal_building_used_time_of_month_by_meter_type(?SOCKET_TYPE, Year, Month),
    cal_building_used_time_of_month_by_meter_type(?FOUR_WAY_SWITCH_TYPE, Year, Month),
    ok.

cal_building_used_time_of_month_by_meter_type(Meter_type, Year, Month)->
    All_meter_of_building_by_type = analyze_util:get_all_meter_of_building_by_meter_type(Meter_type),
    % ?PRINT("~p~n", [All_meter_of_building_by_type]),
    lists:foreach(fun
        ({Building_id, Meter_type_and_meter_list}) ->
            do_work(Building_id, Meter_type_and_meter_list, Meter_type, Year, Month)
    end, All_meter_of_building_by_type).

do_work(Building_id, Meter_type_and_meter_list, Meter_type, Year, Month) ->
    % ?PRINT("~p/~p~n", [Building_id, Meter_type_and_meter_list]),
    case get_avg_used_time_list_of_month(Meter_type_and_meter_list, Year, Month) of
        [] -> ok;
        Meter_avg_used_time_of_month_list ->
            ?PRINT("Meter_avg_used_time_of_month_list:~p~n", [Meter_avg_used_time_of_month_list]),
            {Used_time_level_1_num_str, Used_time_level_2_num_str, Used_time_level_3_num_str, Used_time_level_4_num_str} = cal_Used_time_level_num(Meter_avg_used_time_of_month_list),
            ?PRINT("~p~n", [{Used_time_level_1_num_str, Used_time_level_2_num_str, Used_time_level_3_num_str, Used_time_level_4_num_str}]),
            Year_month_str = lists:flatten(io_lib:format("~4..0w-~2..0w", [Year, Month])),
            New_content = string:join([Year_month_str, Used_time_level_1_num_str, Used_time_level_2_num_str, Used_time_level_3_num_str, Used_time_level_4_num_str], ?FS),
            Used_time_of_month_filepath_of_build = analyze_util:get_used_time_of_month_filepath_of_build(Year, Building_id, Meter_type),
            % ?ERROR("Used_time_of_month_filepath_of_build:~p", [Used_time_of_month_filepath_of_build]),
            filelib:ensure_dir(Used_time_of_month_filepath_of_build),
            case append_to_file(Used_time_of_month_filepath_of_build, New_content) of
                ok -> ok;
                {error, Reason} ->
                    ?ERROR("MODULE:~p append_to_file is error:~p", [?MODULE, Reason])
            end,
            ok
    end,
    ok.

get_avg_used_time_list_of_month(Meter_type_and_meter_list, Year, Month) ->
    Fun = fun
        ({Meter_type, Meter}) ->
            case get_meter_avg_used_time_of_month(Meter_type, Meter, Year, Month) of
                {ok, Meter_avg_used_time_of_day} ->
                    {true, Meter_avg_used_time_of_day};
                {error, Reason} ->
                    false
            end
    end,
    Meter_avg_used_time_of_day_list = lists:filtermap(Fun, Meter_type_and_meter_list),
    Meter_avg_used_time_of_day_list.

get_meter_avg_used_time_of_month(Meter_type, Meter, Year, Month) ->
    Every_day_used_time_of_day_filepath = analyze_util:get_every_day_used_time_filepath(Year, Month, Meter),
    case get_used_time_line_list(Every_day_used_time_of_day_filepath) of
        {ok, Data_line_list} ->
            Fun = fun
                (Data_line) ->
                    case string:tokens(Data_line, ?FS) of
                        [Date_str, Used_time_str] ->
                            {true, list_to_float(Used_time_str)};
                        _ ->
                            false
                    end
            end,
            Used_time_list = lists:filtermap(Fun, Data_line_list),
            % ?PRINT("Used_time_list:~p~n", [Used_time_list]),
            case Used_time_list of
                [] -> {ok, 0.0};
                _ -> 
                    Avg_used_time = lists:sum(Used_time_list) / length(Used_time_list),
                    {ok, ?HELP:floatDecimal(Avg_used_time, 2)}
            end;
        {error, Reason} ->
            ?ERROR("get_used_time_line_list(~p) is error: ~p", [Every_day_used_time_of_day_filepath, Reason]),
            {error, Reason}
    end.

get_used_time_of_date(Date_str, [Data_line | T]) ->
    case string:tokens(Data_line, ?FS) of
        [Date_str, Used_time_str] ->
            {ok, list_to_float(Used_time_str)};
        _ ->
            get_used_time_of_date(Date_str, T)
    end;
get_used_time_of_date(Date_str, []) ->
    {error, not_found}.

get_used_time_line_list(Used_time_of_day_filepath) ->
    case file:read_file(Used_time_of_day_filepath) of
        {ok, Data_bin} ->
            Data_list = binary_to_list(Data_bin),
            Data_line_list = string:tokens(Data_list, ?NL),
            {ok, lists:usort(Data_line_list)};
        {error, Reason} ->
            {error, Reason}
    end.

cal_Used_time_level_num(Meter_used_time_of_day_list) ->
    cal_Used_time_level_num_(Meter_used_time_of_day_list, 0, 0, 0, 0).

cal_Used_time_level_num_([Meter_used_time_of_day | T], Used_time_level_1_num, Used_time_level_2_num, Used_time_level_3_num, Used_time_level_4_num) ->
    if
        0.0 =< Meter_used_time_of_day andalso Meter_used_time_of_day =< 0.0 ->
            cal_Used_time_level_num_(T, Used_time_level_1_num+1, Used_time_level_2_num, Used_time_level_3_num, Used_time_level_4_num);
        0.0 < Meter_used_time_of_day andalso Meter_used_time_of_day =< 4.0 ->
            cal_Used_time_level_num_(T, Used_time_level_1_num, Used_time_level_2_num+1, Used_time_level_3_num, Used_time_level_4_num);
        4.0 < Meter_used_time_of_day andalso Meter_used_time_of_day =< 20.0 ->
            cal_Used_time_level_num_(T, Used_time_level_1_num, Used_time_level_2_num, Used_time_level_3_num+1, Used_time_level_4_num);
        20.0 < Meter_used_time_of_day andalso Meter_used_time_of_day =< 24.0 ->
            cal_Used_time_level_num_(T, Used_time_level_1_num, Used_time_level_2_num, Used_time_level_3_num, Used_time_level_4_num+1);
        true ->
            cal_Used_time_level_num_(T, Used_time_level_1_num, Used_time_level_2_num, Used_time_level_3_num, Used_time_level_4_num)
    end;
cal_Used_time_level_num_([], Used_time_level_1_num, Used_time_level_2_num, Used_time_level_3_num, Used_time_level_4_num) ->
    Used_time_level_1_num_str = integer_to_list(Used_time_level_1_num),
    Used_time_level_2_num_str = integer_to_list(Used_time_level_2_num),
    Used_time_level_3_num_str = integer_to_list(Used_time_level_3_num),
    Used_time_level_4_num_str = integer_to_list(Used_time_level_4_num),
    {Used_time_level_1_num_str, Used_time_level_2_num_str, Used_time_level_3_num_str, Used_time_level_4_num_str}.

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

-module (analyze_building_central_ac_used_time_of_day).

-include("analyze.hrl").
-include("analyze_config.hrl").
-include("analyze_meter.hrl").
-include("print.hrl").

-export([start/0, start/1]).

start() ->
    start(?HELP:date()).

start(Today) ->
    Date = ?HELP:addDay(Today, -1), 
    cal_building_used_time_of_day_by_meter_type(?CENTRAL_AC_TYPE, Date),
    ok.

cal_building_used_time_of_day_by_meter_type(Meter_type, Date) ->
    All_meter_of_building_by_type = analyze_util:get_all_meter_of_building_by_meter_type(Meter_type),
    ?PRINT("~p~n", [All_meter_of_building_by_type]),
    lists:foreach(fun
        ({Building_id, Meter_type_and_meter_list}) ->
            do_work(Building_id, Meter_type_and_meter_list, Meter_type, Date)
    end, All_meter_of_building_by_type).

do_work(Building_id, Meter_type_and_meter_list, Meter_type, Date) ->
    ?PRINT("~p/~p~n", [Building_id, Meter_type_and_meter_list]),
    case get_used_time_list_of_day(Meter_type_and_meter_list, Date) of
        [] -> ok;
        Meter_used_time_of_day_list ->
            ?PRINT("Meter_used_time_of_day_list:~p~n", [Meter_used_time_of_day_list]),
            Fun = 
                fun({Used_time_1, Used_time_2, Used_time_3}, 
                    {Low_speed_used_time, Medium_speed_used_time, High_speed_used_time}) ->
                        {Low_speed_used_time+Used_time_1,
                         Medium_speed_used_time+Used_time_2,
                         High_speed_used_time+Used_time_3}
                end,
            {Low_speed_used_time, Medium_speed_used_time, High_speed_used_time} = 
                lists:foldl(Fun, {0.0, 0.0, 0.0}, Meter_used_time_of_day_list),
            Low_speed_used_time_str = ?HELP:float_to_decimal_str(Low_speed_used_time, 2),
            Medium_speed_used_time_str = ?HELP:float_to_decimal_str(Medium_speed_used_time, 2),
            High_speed_used_time_str = ?HELP:float_to_decimal_str(High_speed_used_time, 2),
            Date_str = ?HELP:dateToStr(Date),
            New_content = string:join([Date_str, Low_speed_used_time_str, Medium_speed_used_time_str, High_speed_used_time_str], ?FS),
            ?PRINT("New_content:~p~n", [New_content]),
            Used_time_of_day_filepath_of_build = analyze_util:get_used_time_of_day_filepath_of_build(Date, Building_id, Meter_type),
            filelib:ensure_dir(Used_time_of_day_filepath_of_build),
            case append_to_file(Used_time_of_day_filepath_of_build, New_content) of
                ok -> ok;
                {error, Reason} ->
                    ?ERROR("MODULE:~p append_to_file is error:~p", [?MODULE, Reason])
            end,
            ok
    end,
    
    ok.

get_used_time_list_of_day(Meter_type_and_meter_list, Date) ->
    Meter_used_time_of_day_list = lists:filtermap(fun
        ({Meter_type, Meter}) ->
            case get_meter_used_time_of_day(Meter_type, Meter, Date) of
                {ok, Meter_used_time_of_day_tuple} ->
                    {true, Meter_used_time_of_day_tuple};
                {error, Reason} ->
                    false
            end
    end, Meter_type_and_meter_list),
    Meter_used_time_of_day_list.

get_meter_used_time_of_day(Meter_type, Meter, Date) ->
    Used_time_of_day_filepath = analyze_util:get_three_speed_used_time_of_day_filepath(Date, Meter),
    case get_used_time_line_list(Used_time_of_day_filepath) of
        {ok, Data_line_list} ->
            ?PRINT("Data_line_list:~p~n", [Data_line_list]),
            Date_str = ?HELP:dateToStr(Date),
            get_used_time_of_date(Date_str, Data_line_list);
        {error, Reason} ->
            ?ERROR("get_used_time_line_list(~p) is error: ~p", [Used_time_of_day_filepath, Reason]),
            {error, Reason}
    end.

get_used_time_of_date(Date_str, [Data_line | T]) ->
    case string:tokens(Data_line, ?FS) of
        [Date_str, Low_speed_used_time_str, Medium_speed_used_time_str, High_speed_used_time_str] ->
            {ok, {list_to_float(Low_speed_used_time_str), list_to_float(Medium_speed_used_time_str), 
             list_to_float(High_speed_used_time_str)}};
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

-module (analyze_building_central_ac_used_time_of_month).

-include("analyze.hrl").
-include("analyze_config.hrl").
-include("analyze_meter.hrl").
-include("print.hrl").

-export([start/0, start/1]).

start() ->
    start(?HELP:date()).

start(Date) ->
    {Year, Month, _} = ?HELP:addDay(Date, -1),
    cal_building_used_time_of_month_by_meter_type(?CENTRAL_AC_TYPE, Year, Month),
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
    case get_used_time_list_of_month(Meter_type_and_meter_list, Year, Month) of
        [] -> ok;
        Meter_used_time_of_month_list ->
            Fun = 
                fun({Used_time_1, Used_time_2, Used_time_3}, 
                    {Low_speed_used_time, Medium_speed_used_time, High_speed_used_time}) ->
                        {Low_speed_used_time+Used_time_1,
                         Medium_speed_used_time+Used_time_2,
                         High_speed_used_time+Used_time_3}
                end,
            {Low_speed_used_time, Medium_speed_used_time, High_speed_used_time} = 
                lists:foldl(Fun, {0.0, 0.0, 0.0}, Meter_used_time_of_month_list),
            Low_speed_used_time_str = ?HELP:float_to_decimal_str(Low_speed_used_time, 2),
            Medium_speed_used_time_str = ?HELP:float_to_decimal_str(Medium_speed_used_time, 2),
            High_speed_used_time_str = ?HELP:float_to_decimal_str(High_speed_used_time, 2),
            Year_and_month_str = ?HELP:year_and_month_str(Year, Month),
            Used_time_str = string:join([Low_speed_used_time_str, Medium_speed_used_time_str, High_speed_used_time_str], ?FS),
            
            Used_time_of_month_filepath_of_build = analyze_util:get_used_time_of_month_filepath_of_build(Year, Building_id, Meter_type),

            case update_used_time(Used_time_of_month_filepath_of_build, Year_and_month_str, Used_time_str) of
                ok -> ok;
                {error, Reason} ->
                    ?ERROR("MODULE:~p append_to_file is error:~p", [?MODULE, Reason])
            end,
            ok
    end,
    ok.

get_used_time_list_of_month(Meter_type_and_meter_list, Year, Month) ->
    Fun = fun
        ({Meter_type, Meter}) ->
            case get_meter_used_time_of_month(Meter_type, Meter, Year, Month) of
                {ok, Meter_used_time_of_month} ->
                    {true, Meter_used_time_of_month};
                {error, Reason} ->
                    false
            end
    end,
    lists:filtermap(Fun, Meter_type_and_meter_list).

get_meter_used_time_of_month(Meter_type, Meter, Year, Month) ->
    Used_time_of_month_filepath = analyze_util:get_three_speed_used_time_of_month_filepath(Year, Meter),
    case get_used_time_line_list(Used_time_of_month_filepath) of
        {ok, Data_line_list} ->
            Year_and_month_str = ?HELP:year_and_month_str(Year, Month),    
            get_used_time_of_month(Year_and_month_str, Data_line_list);
        {error, Reason} ->
            ?ERROR("get_used_time_line_list(~p) is error: ~p", [Used_time_of_month_filepath, Reason]),
            {error, Reason}
    end.

get_used_time_of_month(Year_and_month_str, [Data_line | T]) ->
    case string:tokens(Data_line, ?FS) of
        [Year_and_month_str, Low_speed_used_time_str, Medium_speed_used_time_str, High_speed_used_time_str] ->
            {ok, {list_to_float(Low_speed_used_time_str), list_to_float(Medium_speed_used_time_str), 
             list_to_float(High_speed_used_time_str)}};
        _ ->
            get_used_time_of_month(Year_and_month_str, T)
    end;
get_used_time_of_month(Year_and_month_str, []) ->
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

update_used_time(Used_time_of_month_filepath, Year_and_month_str, Used_time_str) ->
    case get_new_used_ele_content(Used_time_of_month_filepath, Year_and_month_str, Used_time_str) of
        {ok, New_content} ->
            update_to_file(Used_time_of_month_filepath, New_content);
        {error, enoent} ->
            New_content = string:join([Year_and_month_str, Used_time_str], ?FS),
            update_to_file(Used_time_of_month_filepath, New_content);
        {error, Reason} ->
            {error, Reason}
    end.

get_new_used_ele_content(Filepath, Year_and_month_str, Used_time_str) ->
    case file:read_file(Filepath) of
        {ok, Binary} ->
            DataList = string:tokens(binary_to_list(Binary), ?NL),
            List1 = [DataLine || DataLine <- DataList, (not lists:prefix(Year_and_month_str, DataLine))],
            New_line = string:join([Year_and_month_str, Used_time_str], ?FS),
            List2 = lists:reverse([New_line | lists:reverse(List1)]),
            {ok, string:join(List2, ?NL)};
        {error, Reason} ->
            {error, Reason}
    end.

update_to_file(Filepath, New_content) ->
    filelib:ensure_dir(Filepath),
    case file:open(Filepath, [binary, write]) of
        {ok, Fd} ->
            io:fwrite(Fd, "~s~n", [New_content]),
            file:close(Fd),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

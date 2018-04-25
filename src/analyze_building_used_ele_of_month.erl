-module (analyze_building_used_ele_of_month).

-include("analyze.hrl").
-include("analyze_config.hrl").
-include("analyze_meter.hrl").
-include("print.hrl").

-export([start/0, start/1]).

start() ->
    start(?HELP:date()).

start(Date) ->
    {Year, Month} = ?HELP:get_prev_month(Date),
    cal_building_used_ele_of_month_by_meter_type(?AC_TYPE, Year, Month),
    cal_building_used_ele_of_month_by_meter_type(?SOCKET_TYPE, Year, Month),
    cal_building_used_ele_of_month_by_meter_type(?FOUR_WAY_SWITCH_TYPE, Year, Month),
    cal_building_used_ele_of_month_by_meter_type(?CENTRAL_AC_TYPE, Year, Month),
    ok.

cal_building_used_ele_of_month_by_meter_type(Meter_type, Year, Month) ->
    All_meter_of_building_by_type = analyze_util:get_all_meter_of_building_by_meter_type(Meter_type),
    ?PRINT("~p~n", [All_meter_of_building_by_type]),
    lists:foreach(fun
        ({Building_id, Meter_type_and_meter_list}) ->
            do_work(Building_id, Meter_type_and_meter_list, Meter_type, Year, Month)
    end, All_meter_of_building_by_type).

do_work(Building_id, Meter_type_and_meter_list, Meter_type, Year, Month) ->
    Used_ele_of_month = get_all_used_ele_of_month(Meter_type_and_meter_list, Year, Month),
    Used_ele_of_month_str = ?HELP:float_to_decimal_str(Used_ele_of_month, 2),
    Year_month_str = lists:flatten(io_lib:format("~4..0w-~2..0w", [Year, Month])),
    New_content = string:join([Year_month_str, Used_ele_of_month_str], ?FS),
    ?PRINT("New_content:~p~n", [New_content]),
    Used_ele_of_month_filepath_of_build = analyze_util:get_used_ele_of_month_filepath_of_build(Year, Building_id, Meter_type),
    filelib:ensure_dir(Used_ele_of_month_filepath_of_build),
    case append_to_file(Used_ele_of_month_filepath_of_build, New_content) of
        ok -> ok;
        {error, Reason} ->
            ?ERROR("MODULE:~p append_to_file is error:~p", [?MODULE, Reason])
    end,
    ok.

get_all_used_ele_of_month(Meter_type_and_meter_list, Year, Month) ->
    Meter_used_ele_of_day_list = lists:filtermap(fun
        ({Meter_type, Meter}) ->
            case get_meter_used_ele_of_month(Meter_type, Meter, Year, Month) of
                {ok, Meter_used_ele_of_day} ->
                    {true, Meter_used_ele_of_day};
                {error, Reason} ->
                    false
            end
    end, Meter_type_and_meter_list),
    case Meter_used_ele_of_day_list of
        [] ->
            0.0;
        _ ->
            lists:sum(Meter_used_ele_of_day_list)
    end.

get_meter_used_ele_of_month(Meter_type, Meter, Year, Month) ->
    Used_ele_of_month_filepath = analyze_util:get_used_ele_of_month_filepath(Year, Meter),
    case get_used_ele_line_list(Used_ele_of_month_filepath) of
        {ok, Data_line_list} ->
            Year_month_str = lists:flatten(io_lib:format("~4..0w-~2..0w", [Year, Month])),
            Meter_used_ele_of_month = get_used_ele_of_year_and_month(Year_month_str, Data_line_list),
            {ok, Meter_used_ele_of_month};
        {error, Reason} ->
            ?ERROR("get_used_ele_line_list(~p) is error: ~p", [Used_ele_of_month_filepath, Reason]),
            {error, Reason}
    end.

get_used_ele_line_list(Used_ele_of_day_filepath) ->
    case file:read_file(Used_ele_of_day_filepath) of
        {ok, Data_bin} ->
            Data_list = binary_to_list(Data_bin),
            Data_line_list = string:tokens(Data_list, ?NL),
            {ok, lists:usort(Data_line_list)};
        {error, Reason} ->
            {error, Reason}
    end.

get_used_ele_of_year_and_month(Year_month_str, [Data_line | T]) ->
    case string:tokens(Data_line, ?FS) of
        [Year_month_str, Used_ele_str] ->
            list_to_float(Used_ele_str);
        _ ->
            get_used_ele_of_year_and_month(Year_month_str, T)
    end;
get_used_ele_of_year_and_month(Year_month_str, []) ->
    0.0.

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


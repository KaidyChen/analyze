-module (analyze_building_used_ele_of_month_2).

-include("analyze.hrl").
-include("analyze_config.hrl").
-include("analyze_meter.hrl").
-include("print.hrl").

-export([start/0, start/1]).

start() ->
    start(?HELP:date()).

start(Date) ->
    {Year, Month, _} = ?HELP:addDay(Date, -1),
    cal_building_used_ele_of_month(Year, Month),
    ok.

cal_building_used_ele_of_month(Year, Month) ->
    All_meter_of_building = analyze_util:get_type_and_meter_and_master_label_of_building_by_meter_type_list(?USED_ELE_METER_TYPE_LIST),
    ?PRINT("~p~n", [All_meter_of_building]),
    lists:foreach(fun
        ({Building_id, Meter_type_and_meter_and_master_label_list}) ->
            do_work(Building_id, Meter_type_and_meter_and_master_label_list, Year, Month)
    end, All_meter_of_building).

do_work(Building_id, Meter_type_and_meter_and_master_label_list, Year, Month) ->
    case get_master_label_used_ele_of_month(Meter_type_and_meter_and_master_label_list, Year, Month) of
        [] ->
            ok;
        Master_label_used_ele_list ->
    	    Fun = 
	        fun({_, Used_ele}, Sum) ->
                 	 Sum + Used_ele
                end,		
    	    Sum_used_ele = lists:foldl(Fun, 0.0, Master_label_used_ele_list),
 
            Year_month_str = lists:flatten(io_lib:format("~4..0w-~2..0w", [Year, Month])),
            Subentry_used_ele = binary_to_list(jsx:encode([{<<"all">>, Sum_used_ele} | Master_label_used_ele_list])),
            ?PRINT("Subentry:~s~n", [Subentry_used_ele]),
            update_subentry_used_ele(Year, Building_id, Year_month_str, Subentry_used_ele),
	    ?PRINT("SUM:~p~n", [Sum_used_ele]),
            Used_ele_of_month_str = ?HELP:float_to_decimal_str(Sum_used_ele, 2),
            ?PRINT("Used_ele_of_month:~p~n", [Used_ele_of_month_str]),
            update_all_used_ele(Year, Building_id, Year_month_str, Used_ele_of_month_str),
            ok
    end.

get_master_label_used_ele_of_month(Meter_type_and_meter_and_master_label_list, Year, Month) ->
    Meter_used_ele_of_day_list = lists:filtermap(fun
        ({Meter_type, Meter, Master_label}) ->
            case get_meter_used_ele_of_month(Meter_type, Meter, Year, Month) of
                {ok, Meter_used_ele_of_day} ->
                    {true, {Master_label, Meter_used_ele_of_day}};
                {error, Reason} ->
                    false
            end
    end, Meter_type_and_meter_and_master_label_list),
    Keys = proplists:get_keys(Meter_used_ele_of_day_list),
    [{list_to_binary(Key), lists:sum(proplists:get_all_values(Key, Meter_used_ele_of_day_list))} || 
        Key <- proplists:get_keys(Meter_used_ele_of_day_list)].
    

get_meter_used_ele_of_month(Meter_type, Meter, Year, Month) ->
    Used_ele_of_month_filepath = analyze_util:get_used_ele_of_month_filepath(Year, Meter),
    case get_used_ele_line_list(Used_ele_of_month_filepath) of
        {ok, Data_line_list} ->
            Year_month_str = lists:flatten(io_lib:format("~4..0w-~2..0w", [Year, Month])),
            Meter_used_ele_of_month = get_used_ele_of_year_and_month(Year_month_str, Data_line_list),
            {ok, Meter_used_ele_of_month};
        {error, Reason} ->
            ?PRINT("get_used_ele_line_list(~p) is error: ~p~n", [Used_ele_of_month_filepath, Reason]),
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

get_new_used_ele_content(Used_ele_of_month_filepath, Year_and_month_str, Used_ele_str) ->
    case file:read_file(Used_ele_of_month_filepath) of
        {ok, Binary} ->
            DataList = string:tokens(binary_to_list(Binary), ?NL),
            List1 = [DataLine || DataLine <- DataList, (not lists:prefix(Year_and_month_str, DataLine))],
            New_line = string:join([Year_and_month_str, Used_ele_str], ?FS),
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

update_subentry_used_ele(Year, Building_id, Year_month_str, Subentry_used_ele) ->
    Subentry_used_ele_of_month_filepath_of_build = 
        analyze_util:get_subentry_used_ele_of_month_filepath_of_build(Year, Building_id),
    filelib:ensure_dir(Subentry_used_ele_of_month_filepath_of_build),
    case get_new_used_ele_content(Subentry_used_ele_of_month_filepath_of_build, Year_month_str, Subentry_used_ele) of
        {ok, New_content} ->
            ?PRINT("~p~n", [New_content]),
            case update_to_file(Subentry_used_ele_of_month_filepath_of_build, New_content) of
                ok -> ok;
                {error, Reason} ->
                    ?ERROR("append_to_file ~p is error:~p", [Subentry_used_ele_of_month_filepath_of_build, Reason])
            end;
        {error, enoent} ->
            New_content = string:join([Year_month_str, Subentry_used_ele], ?FS),
            case update_to_file(Subentry_used_ele_of_month_filepath_of_build, New_content) of
                ok -> ok;
                {error, Reason} ->
                    ?ERROR("append_to_file ~p is error:~p", [Subentry_used_ele_of_month_filepath_of_build, Reason])
            end;
        {error, What} ->
            ?ERROR("get_new_used_ele_content ~p is error:~p", [Subentry_used_ele_of_month_filepath_of_build, What])
    end.

update_all_used_ele(Year, Building_id, Year_month_str, Used_ele_of_month_str) ->
    Used_ele_of_month_filepath_of_build = 
        analyze_util:get_used_ele_of_month_filepath_of_build(Year, Building_id),
    filelib:ensure_dir(Used_ele_of_month_filepath_of_build),
    case get_new_used_ele_content(Used_ele_of_month_filepath_of_build, Year_month_str, Used_ele_of_month_str) of
        {ok, New_content} ->
            ?PRINT("~p~n", [New_content]),
            case update_to_file(Used_ele_of_month_filepath_of_build, New_content) of
                ok -> ok;
                {error, Reason} ->
                    ?ERROR("append_to_file ~p is error:~p", [Used_ele_of_month_filepath_of_build, Reason])
            end;
        {error, enoent} ->
            New_content = string:join([Year_month_str, Used_ele_of_month_str], ?FS),
            case update_to_file(Used_ele_of_month_filepath_of_build, New_content) of
                ok -> ok;
                {error, Reason} ->
                    ?ERROR("append_to_file ~p is error:~p", [Used_ele_of_month_filepath_of_build, Reason])
            end;
        {error, What} ->
            ?ERROR("get_new_used_ele_content ~p is error:~p", [Used_ele_of_month_filepath_of_build, What])
    end.








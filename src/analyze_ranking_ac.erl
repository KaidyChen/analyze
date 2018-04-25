-module (analyze_ranking_ac).

-include("analyze.hrl").
-include("analyze_config.hrl").
-include("print.hrl").

-export([start/0, start/1]).

start() ->
    Date = ?HELP:date(),
    start(Date).

start(Date) ->
    case get_ac_meter_list() of
        [] -> ok;
        Ac_meter_list ->
            io:format("Ac_meter_list:~p--~n", [Ac_meter_list]),
            Fun = fun
                (Meter) ->
                    case get_meter_used_and_save_day_num(Meter, Date) of
                        {ok, {0, _Save_ele_day_num, _Save_ratio}} ->    % 用电天数为0，不参与排名
                            false;
                        {ok, {Used_ele_day_num, Save_ele_day_num, Save_ratio}} ->
                            {true, {Meter, Used_ele_day_num, Save_ele_day_num, Save_ratio}};
                        {error, Reason} ->
                            false
                    end
            end,
            Un_sorted_list = lists:filtermap(Fun, Ac_meter_list),
            case Un_sorted_list of
                [] -> ok;
                _ ->
                    Sorted_List = lists:reverse(lists:keysort(4, Un_sorted_list)),     % 以Sort_ratio排序
                    case cal_ranking(Sorted_List) of
                        [] -> ok;
                        Ranking_list ->
                            SaveFilePath = analyze_util:get_ranking_filepath(Date),
                            filelib:ensure_dir(SaveFilePath),
                            Content = string:join(Ranking_list, ?NL),
                            case update_ranking(SaveFilePath, Content) of
                                ok -> ok;
                                {error, Reason} ->
                                    ?ERROR("update_ranking is error:~p", [Reason])
                            end   
                    end 
            end
    end.

get_ac_meter_list() ->
    case analyze_meter_util:select_meter_by_meter_type(?AC_TYPE) of
        [] -> [];
        Ac_meter_list ->    
            Fun = fun
                (Item) ->
                    [Meter | _] = Item,
                    {true, Meter}
            end,
            lists:filtermap(Fun, Ac_meter_list)
    end.

get_meter_used_and_save_day_num(Meter, Date) ->
    case get_hour_avg_used_ele_list(Meter, Date) of
        {ok, Hour_avg_used_ele_list} ->
            Used_ele_day_list = [Avg_hour_used_ele_float || Avg_hour_used_ele_float <- Hour_avg_used_ele_list, Avg_hour_used_ele_float > 0.0],
            Used_ele_day_num = length(Used_ele_day_list),
            Save_ele_day_list = [Avg_hour_used_ele_float || Avg_hour_used_ele_float <- Hour_avg_used_ele_list, Avg_hour_used_ele_float > 0.0, Avg_hour_used_ele_float < ?SAVESTANDARD],
            Save_ele_day_num = length(Save_ele_day_list),
            Save_ratio = case (Used_ele_day_num  =:= 0) of
                true -> 0.0;
                false -> ?HELP:floatDecimal(Save_ele_day_num / Used_ele_day_num, 2)
            end,
            {ok, {Used_ele_day_num, Save_ele_day_num, Save_ratio}};
        {error, Reason} -> {error, Reason}
    end.

get_hour_avg_used_ele_list(Meter, Date) ->
    {Year, Month, _} = Date,
    Filepath = analyze_hour_avg_usedele:get_avg_hour_usedele_filepath(Year, Month, Meter),
    case file:read_file(Filepath) of
       {ok, Data_bin} ->
            Data_bitstring = binary_to_list(Data_bin),
            Data_line_list = string:tokens(Data_bitstring, ?NL),
            Fun = fun
                (Data_line) ->
                    %% 2016-09-18 1.24
                    [_, Avg_hour_used_ele_str] = string:tokens(Data_line, ?FS),
                    Avg_hour_used_ele_float = list_to_float(Avg_hour_used_ele_str),
                    {true, Avg_hour_used_ele_float}
            end,
            New_data_line_list = lists:filtermap(Fun, lists:usort(Data_line_list)),
            {ok, New_data_line_list};
        {error, Reason} -> {error, Reason}
    end.

cal_ranking(Sorted_List = [H | _]) ->
    Fun_1 = fun(X, {{Pre, Pre_rank, Count}, Acc}) -> 
        {_Meter, _Used_ele_day_num, _Save_ele_day_num, Save_ratio} = X,
        {_, _, _, SaveRatio_pre} = Pre,
        New_count = Count + 1,
        if 
            (Save_ratio =:= SaveRatio_pre ) -> 
                {{X, Pre_rank, New_count}, [{Pre_rank, X} | Acc]}; 
            true ->  
                {{X, New_count, New_count}, [{New_count, X} | Acc]} 
        end 
    end,
    {{_, _, RankMax}, Save_Ranking_list} = lists:foldl(Fun_1, {{H, 1, 0}, []}, Sorted_List),
    case RankMax of
        0 -> [];
        _ -> 
            Fun_2 = fun(Item) ->
                    {Value, {Meter, Used_ele_day_num, Save_ele_day_num, Save_ratio}} = Item,
                    Return_value = string:join([Meter, integer_to_list(Used_ele_day_num), integer_to_list(Save_ele_day_num), float_to_list(Save_ratio, [{decimals, 2}]), integer_to_list(Value), float_to_list((RankMax-Value)/RankMax, [{decimals, 2}])], ?FS),
                    {true, Return_value}
            end,
            lists:filtermap(Fun_2, Save_Ranking_list)
    end.

update_ranking(FilePath, Content) ->
    case file:open(FilePath, [write, binary]) of
        {ok, Fd} ->
            io:fwrite(Fd, "~s~n", [Content]),
            file:close(Fd),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.



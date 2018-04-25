-module(rpc_api).

-include("analyze.hrl").
-include("analyze_config.hrl").
-include("print.hrl").
-include("batch_task.hrl").
-include("time_table.hrl").

-compile(export_all).

-define(MAXIMONTH, 12).

to_binary(Integer) when is_integer(Integer) ->
    integer_to_binary(Integer);
to_binary(List) when is_list(List) ->
    list_to_binary(List);
to_binary(Binary) when is_binary(Binary) ->
    Binary;
to_binary(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, utf8).


%%------------------------------------------------------------------------------------------------
%% 通讯质量
%%------------------------------------------------------------------------------------------------
get_cq_of_gateway(Gateway_type, Gateway) ->
    case analyze_meter_field_store:get_meter_and_type_list_by_gateway(Gateway) of
        {ok, Meter_and_type_list} ->
            Fun = fun
                ([Meter, Meter_type]) ->
                    Meter_and_cq = get_cq_of_meter_(Meter_type, Meter)
            end,
            Meter_and_cq_list = lists:map(Fun, Meter_and_type_list),
            to_binary(string:join(Meter_and_cq_list, ?NL));
        {error, Why} -> <<?NULL>>  
    end.

get_cq_of_meter(Meter_type, Meter) when is_list(Meter) ->
    Gateway_status = case analyze_meter_field_store:get_gateway(Meter, Meter_type) of
        {ok, Gateway} ->
            case analyze_gateway_status:lookup(Gateway) of
                {ok, Status} ->
                    Status;
                {error, _} ->
                    ?NULL
            end;
        {error, Reason} ->
            ?NULL
    end,
    Meter_cq = get_cq_of_meter_(Meter_type, Meter),
    to_binary(lists:concat([Meter_cq, " ", Gateway_status])).

get_cq_of_meter_(Meter_type, Meter) ->
    Cq_of_meter = case analyze_meter_util:get_cq(Meter_type, Meter) of
        {ok, Cq} ->
            integer_to_list(Cq);
        {error, Why} ->
            ?NULL
    end,
    string:join([Meter, Cq_of_meter], ?FS).
    

%%------------------------------------------------------------------------------------------------
%% 实时数据块
%%------------------------------------------------------------------------------------------------
get_meter_current_blob(Meter_type,  Meter) ->
    case analyze_meter_to_blob:lookup({Meter_type,  Meter}) of
        {ok, Current_Blob} ->
            analyze_meter_blob_util:get_blob_field_str(Current_Blob);
        {error, Reason} ->
            ?ERROR("~p:get_socket_readtime_workflow(~p) is error:~p", [?MODULE, Meter, Reason]),
            <<?NULL>>
    end.

%%------------------------------------------------------------------------------------------------
%% 获取30个点实时数据点
%%------------------------------------------------------------------------------------------------
get_30_data_point(Meter_type, Meter) ->
    Filepath = task_util:get_realtime_data_point_filepath(Meter),
    case file:read_file(Filepath) of
        {ok, Data_Bin} ->
            Data_Bin;
       {error, Reason} -> 
            ?ERROR("~p:get_readtime_data_points read_file(~p) is error:~p", [?MODULE, Filepath, Reason]),
            <<?NULL>>
    end.

%%------------------------------------------------------------------------------------------------
%% 获取48小时
%%------------------------------------------------------------------------------------------------
get_48hour(?AC_TYPE, Meter) ->
    Usedele_48hour = get_48hour_usedele(Meter),
    Avg_temp_48hour = get_48hour_avg_temp(Meter),
    list_to_binary(string:join([Usedele_48hour, Avg_temp_48hour], "#"));
get_48hour(?SOCKET_TYPE, Meter) ->
    Usedele_48hour = get_48hour_usedele(Meter),
    list_to_binary(Usedele_48hour);
get_48hour(?FOUR_WAY_SWITCH_TYPE, Meter) ->
    Usedele_48hour = get_48hour_usedele(Meter),
    list_to_binary(Usedele_48hour);
get_48hour(?CENTRAL_AC_TYPE, Meter) ->
    Used_time_48hour = get_48hour_used_time(Meter),
    Avg_temp_48hour = get_48hour_avg_temp(Meter),
    list_to_binary(string:join([Used_time_48hour, Avg_temp_48hour], "#"));
get_48hour(_, _) -> <<?NULL>>.

get_48hour_usedele(Meter) ->
    Filepath = analyze_48hour_usedele:get_48hour_usedele_filepath(Meter),
    case file:read_file(Filepath) of
        {ok, Data_Bin} ->
            string:strip(binary_to_list(Data_Bin), right, $\n);
        {error, Reason} -> 
            ?ERROR("~p:get_48hour_usedele read_file(~p) is error:~p", [?MODULE, Filepath, Reason]),
            ?NULL
    end.

get_48hour_avg_temp(Meter) ->
    Filepath = analyze_48hour_avg_temp:get_48hour_avgtemp_filepath(Meter),
    case file:read_file(Filepath) of
        {ok, Data_Bin} ->
            string:strip(binary_to_list(Data_Bin), right, $\n);
        {error, Reason} -> 
            ?ERROR("~p:get_48hour_avg_temp read_file(~p) is error:~p", [?MODULE, Filepath, Reason]),
            ?NULL
    end.

get_48hour_used_time(Meter) ->
    Filepath = analyze_48hour_central_ac_used_time:get_48hour_used_time_filepath(Meter),
    case file:read_file(Filepath) of
        {ok, Data_Bin} ->
            string:strip(binary_to_list(Data_Bin), right, $\n);
        {error, Reason} -> 
            ?ERROR("~p:get_48hour_used_time read_file(~p) is error:~p", [?MODULE, Filepath, Reason]),
            ?NULL
    end.

%%------------------------------------------------------------------------------------------------
%% 工作活动
%%------------------------------------------------------------------------------------------------
get_work_activities(Meter_type, Meter) ->
    Today_work_activities = case analyze_meter_work_activities_store:lookup(Meter_type, Meter) of
        {ok, Work_activities} ->
            analyze_meter_work_activities_util:get_work_activities_str(Meter_type, Work_activities);
        {error, _} ->
            ?NULL
    end,
    Today = ?HELP:date(),
    {Yesterday_year, Yesterday_month, _} = Yesterday = ?HELP:addDay(Today, -1),
    {Before_yesterday_year, Before_yesterday_month, _} = Before_yesterday = ?HELP:addDay(Today, -2),
    Yesterday_year_month = {Yesterday_year, Yesterday_month},
    Before_yesterday_year_month = {Before_yesterday_year, Before_yesterday_month},
    History_list = case (Yesterday_year_month =:= Before_yesterday_year_month) of
        %% 两天是同一个月的
        true ->
            Filepath = analyze_meter_work_activities_util:get_work_activities_filepath(Yesterday_year, Yesterday_month, Meter),
            get_work_activities_list_by_filepath(Filepath);
        %% 两个月的
        false ->
            Filepath1 = analyze_meter_work_activities_util:get_work_activities_filepath(Yesterday_year, Yesterday_month, Meter),
            List1 = get_work_activities_list_by_filepath(Filepath1),
            Filepath2 = analyze_meter_work_activities_util:get_work_activities_filepath(Before_yesterday_year, Before_yesterday_month, Meter),
            List2 = get_work_activities_list_by_filepath(Filepath2),
            List1 ++ List2
    end,
    Today_str = ?HELP:dateToStr(Today),
    Yesterday_str = ?HELP:dateToStr(Yesterday),
    Before_yesterday_str = ?HELP:dateToStr(Before_yesterday),
    Today_data = string:join([Today_str, Today_work_activities], ?FS),
    Yesterday_data_init = string:join([Yesterday_str, ?NULL], ?FS), 
    Before_yesterday_data_init = string:join([Before_yesterday_str, ?NULL], ?FS),
    Fun = fun
        (Data_line, {Yesterday_data_tmp, Before_yesterday_data_tmp}) ->
            case string:tokens(Data_line, ?FS) of
                [Yesterday_str, Work_activities_str] ->
                    {Data_line, Before_yesterday_data_tmp};
                [Before_yesterday_str, Work_activities_str] ->
                    {Yesterday_data_tmp, Data_line};
                _ ->
                    {Yesterday_data_tmp, Before_yesterday_data_tmp}
            end
    end,
    {Yesterday_data, Before_yesterday_data} = lists:foldl(Fun, {Yesterday_data_init, Before_yesterday_data_init}, History_list),
    to_binary(string:join([Today_data, Yesterday_data, Before_yesterday_data], ?NL)).

get_work_activities_list_by_filepath(Filepath) ->
    case file:read_file(Filepath) of
        {ok, Data_bin} ->
            Data_bitstring = binary_to_list(Data_bin),
            Data_line_list = string:tokens(Data_bitstring, ?NL),
            lists:usort(Data_line_list);
        {error, Reason} -> []
    end.

%%------------------------------------------------------------------------------------------------
%% 实时数据流
%%------------------------------------------------------------------------------------------------
get_socket_readtime_workflow(Meter_type, Meter) ->
    case analyze_realtime_workflow:lookup(Meter_type, Meter) of
        {ok, Readtime_workflow} ->
            to_binary(analyze_workflow_pc:get_realtime_workflow_str(Readtime_workflow));
        {error, Reason} ->
            ?ERROR("~p:get_socket_readtime_workflow(~p) is error:~p", [?MODULE, Meter, Reason]),
            <<?NULL>>
    end.

%%------------------------------------------------------------------------------------------------
%% 活动流近段时间记录
%%------------------------------------------------------------------------------------------------
get_socket_workflow_history(Meter_type, Meter) ->  
    case get_socket_workflow_history_list(Meter) of
        [] -> ?NULL;
        List -> list_to_binary(string:join(List, ?NL))
    end. 
    
get_socket_workflow_history_list(Meter) ->
    {Today_year, Today_month, _} = Today = ?HELP:date(),
    {Yesterday_year, Yesterday_month, _} = Yesterday = ?HELP:addDay(Today, -1),
    {Before_yesterday_year, Before_yesterday_month, _} = Before_yesterday = ?HELP:addDay(Today, -2),
    Today_year_month = {Today_year, Today_month},
    Yesterday_year_month = {Yesterday_year, Yesterday_month},
    Before_yesterday_year_month = {Before_yesterday_year, Before_yesterday_month},
    History_list = case {(Today_year_month =:= Yesterday_year_month), (Yesterday_year_month =:= Before_yesterday_year_month)} of
        %% 三天是同一个月的
        {true, true} ->
            Filepath = analyze_workflow_util:get_workflow_filepath(Today_year, Today_month, Meter),
            get_list_by_filepath(Filepath);
        %% 两个月的
        _ ->
            Filepath1 = analyze_workflow_util:get_workflow_filepath(Today_year, Today_month, Meter),
            List1 = get_list_by_filepath(Filepath1),
            Filepath2 = analyze_workflow_util:get_workflow_filepath(Before_yesterday_year, Before_yesterday_month, Meter),
            List2 = get_list_by_filepath(Filepath2),
            List1 ++ List2
    end,
    % ?PRINT("~p~n", [History_list]),
    Today_str = ?HELP:dateToStr(Today),
    Yesterday_str = ?HELP:dateToStr(Yesterday),
    Before_yesterday_str = ?HELP:dateToStr(Before_yesterday),
    N = length(Today_str),
    {List_of_today, List_of_yesterday, List_of_before_yesterday} = lists:foldl(fun
        (Data_line, {Today_list_tmp, Yesterday_list_tmp, Before_yesterday_list_tmp}) ->
            case lists:split(N, Data_line) of
                {Today_str, Time_and_status} ->
                    {[strip(Time_and_status) | Today_list_tmp], Yesterday_list_tmp, Before_yesterday_list_tmp};
                {Yesterday_str, Time_and_status} ->
                    {Today_list_tmp, [strip(Time_and_status) | Yesterday_list_tmp], Before_yesterday_list_tmp};
                {Before_yesterday_str, Time_and_status} ->
                    {Today_list_tmp, Yesterday_list_tmp, [strip(Time_and_status) | Before_yesterday_list_tmp]};
                _ ->
                    {Today_list_tmp, Yesterday_list_tmp, Before_yesterday_list_tmp}
            end
    end,  {[], [], []}, History_list),
    Today_list = concat(Today_str, List_of_today),
    Yesterday_list = concat(Yesterday_str, List_of_yesterday),
    Before_yesterday_list = concat(Before_yesterday_str, List_of_before_yesterday),
    [ X || X <- [Today_list, Yesterday_list, Before_yesterday_list], X =/= false].

strip(Str) ->
    string:strip(Str).

concat(Day_str, List_of_day) when is_list(List_of_day) ->
    case List_of_day of
        [] -> false;
        _ -> lists:concat([Day_str, "/", string:join(List_of_day, "#")])
    end.
    
get_list_by_filepath(Filepath) ->
    case file:read_file(Filepath) of
        {ok, Data_bin} ->
            Data_bitstring = binary_to_list(Data_bin),
            Data_line_list = string:tokens(Data_bitstring, ?NL),
            lists:usort(Data_line_list);
        {error, Reason} -> []
    end.

%%------------------------------------------------------------------------------------------------
%% 获取周循环任务
%%------------------------------------------------------------------------------------------------
get_cycle_tasks(Meter_type, Meter) ->
    Filepath = analyze_meter_cycle_task:getFilePath(Meter),
    case file:read_file(Filepath) of
        {ok, Data_bin} ->
            Data_bin;
        {error, Reason} -> 
            ?ERROR("~p:get_cycle_tasks read_file(~p) is error:~p", [?MODULE, Filepath, Reason]),
            <<?NULL>>
    end.

%%------------------------------------------------------------------------------------------------
%% 获取表的定额数据
%%------------------------------------------------------------------------------------------------
get_quota(Meter_type, Meter) ->
    case analyze_meter_quota_server:get_quota_and_mode(Meter_type, Meter) of
        {ok, Quota_str, Residues_quantity_str, Mode_str} ->
            to_binary(string:join([Quota_str, Mode_str, Residues_quantity_str], "#"));
        {error, _} ->
            <<?NULL>>
    end.

%% 是否存在未知的充值记录
is_exist_unknown_of_recharge(Meter_type, Meter) ->
    case analyze_meter_recharge:is_exist_unknown_of_recharge(Meter_type, Meter) of
        true ->
            to_binary("true");
        false ->
            to_binary("false")
    end.

%%------------------------------------------------------------------------------------------------
%% 空调月度使用数据
%%------------------------------------------------------------------------------------------------
get_used_data_of_month(Meter_type, Meter, Year_and_month_str_tmp) ->
    [Year_str, Month_str] = string:tokens(Year_and_month_str_tmp, "-"),
    Year = list_to_integer(Year_str), 
    Month = list_to_integer(Month_str),
    Filepath = analyze_util:get_used_data_of_month_filepath(Year, Meter),
    case file:read_file(Filepath) of
        {ok, Binary} ->
            Year_and_month_str = ?HELP:year_and_month_str(Year, Month),
            Data_line_list = string:tokens(binary_to_list(Binary), ?NL),
            case get_data_line_of_year_month(lists:reverse(Data_line_list), Year_and_month_str) of
                {ok, Data_line} ->
                    to_binary(Data_line);
                {error, _} ->
                    <<?NULL>>
            end;
        {error, _} ->
            <<?NULL>>
    end.

get_data_line_of_year_month([Data_line | List], Year_and_month_str) ->
    case string:tokens(Data_line, ?FS) of
        [Year_and_month_str | _] ->
            {ok, Data_line};
        _ ->
            get_data_line_of_year_month(List, Year_and_month_str)
    end;
get_data_line_of_year_month(_, _) ->
    {error, not_found}.


%%------------------------------------------------------------------------------------------------
%% 获取排行榜
%%------------------------------------------------------------------------------------------------
get_ranking(Meters) ->
    case get_meter_list(Meters) of
        [] -> <<?NULL>>;
        Meter_list ->
            case analyze_util:get_ranking_list() of
                {ok, Data_line_list} ->
                    Pred = fun(Data_line) ->
                        [Meter | _] = string:tokens(Data_line, ?FS),
                        lists:member(Meter, Meter_list)
                    end,
                    % ?PRINT("~p~n", [Data_line_list]),
                    Ranking_list = lists:filter(Pred, Data_line_list),
                    % ?PRINT("~p~n", [Ranking_list]),
                    list_to_binary(string:join(Ranking_list, ?NL));
                {error, Reason} -> 
                    ?ERROR("MODULE:~p get_ranking_list is error:~p", [?MODULE, Reason]),
                    <<?NULL>>
            end
    end.

get_meter_list(Meters) ->
    string:tokens(Meters, ",").

%%------------------------------------------------------------------------------------------------
%% 获取建筑能源
%%------------------------------------------------------------------------------------------------
get_energy_by_building_id(Building_id) ->
    case analyze_virtual_build:get_energy_by_building_id(Building_id) of
        {ok, Energy_json} ->
            Energy_json;
        {error, Reason} ->
            ?ERROR("get_energy_by_building_id(~p) is error:~p", [Building_id, Reason]),
            <<?NULL>>
    end.

%%------------------------------------------------------------------------------------------------
%% 根据条件获取批量任务
%%------------------------------------------------------------------------------------------------
get_batch_task_by_condition(Condition) ->
    case analyze_batch_task_store:get_batch_task_by_condition(Condition) of
        {error, not_found} -> <<?NULL>>;
        {ok, Batch_task_list} -> 
            Fun = fun
                (Batch_task) ->
                    #batch_task{
                        task_id = Task_id, 
                        task_status = Task_status,
                        datetime_start = Datetime_start,
                        datetime_end = Datetime_end,
                        operation_type = Operation_type,
                        operation_argv = Operation_argv
                    } = Batch_task,
                Datetime_start_str = ?HELP:getDateTimeStr(Datetime_start),
                Datetime_end_str = ?HELP:getDateTimeStr(Datetime_end),
                string:join([Task_id, integer_to_list(Task_status), Datetime_start_str, Datetime_end_str, Operation_type], "#")
            end,
            Tasks_str_list = lists:map(Fun, Batch_task_list),
            Tasks = string:join(Tasks_str_list, ?NL),
            list_to_binary(Tasks)
    end.

%%------------------------------------------------------------------------------------------------
%% 楼宇的日用电量
%%------------------------------------------------------------------------------------------------
get_building_used_ele_of_day(Building_id, Meter_type, Year_and_month_str) ->
    [Year_str, Month_str] = string:tokens(Year_and_month_str, "-"),
    Year = list_to_integer(Year_str), 
    Month = list_to_integer(Month_str),
    case get_building_used_ele_list_of_day(Building_id, Meter_type, Year, Month) of
        {ok, Data_line_list} ->
            Sorted_date_line_list = lists:sort(Data_line_list),
            Last_day = ?HELP:last_day_of_the_month(Year, Month),
            Init_list = [{?HELP:dateToStr({Year, Month, Day}), " "} || Day <- lists:seq(1, Last_day)],
            Init_map = maps:from_list(Init_list),
            Fun = fun
                (Data_line, Map) ->
                    case string:tokens(Data_line, ?FS) of
                        [Date_str, Used_ele_str] ->
                            case maps:is_key(Date_str, Map) of
                                true -> 
                                    New_map = maps:update(Date_str, Used_ele_str, Map),
                                    New_map;
                                false ->
                                    Map 
                            end;
                        _ ->
                            Map
                    end
            end,
            Map_tmp = lists:foldl(Fun, Init_map, Data_line_list),
            Values = maps:values(Map_tmp),
            Values_str = string:join(Values, ","),
            list_to_binary(Values_str);
        {error, Reason} ->
            ?ERROR("get_used_ele_list(~p) is error:~p", [Building_id, Reason]),
            <<?NULL>>
    end.

get_building_used_ele_list_of_day(Building_id, Meter_type, Year, Month) ->
    Every_day_used_ele_filepath_of_build = get_every_day_used_ele_filepath_of_build(Year, Month, Building_id, Meter_type), 
    case file:read_file(Every_day_used_ele_filepath_of_build) of
        {ok, Data_bin} ->
            Data_bitstring = binary_to_list(Data_bin),
            Data_line_list = string:tokens(Data_bitstring, ?NL),
            {ok, lists:usort(Data_line_list)};
        {error, Reason} -> {error, Reason}
    end.  

get_every_day_used_ele_filepath_of_build(Year, Month, Build, Meter_type) ->
    analyze_util:get_used_ele_of_day_filepath_of_build(Year, Month, Build, Meter_type).

%%------------------------------------------------------------------------------------------------
%% 楼宇的12个月用电量
%%------------------------------------------------------------------------------------------------
get_building_used_ele_of_month(Building_id) ->
    {Year_1, _, _} = ?HELP:date(),
    Year_2 = Year_1 - 1,
    Used_ele_of_month_filepath_of_build_1 = 
        analyze_util:get_used_ele_of_month_filepath_of_build(Year_1, Building_id),
    Used_ele_of_month_filepath_of_build_2 = 
        analyze_util:get_used_ele_of_month_filepath_of_build(Year_2, Building_id),
    Result = 
        case {get_file_line(Used_ele_of_month_filepath_of_build_1), 
              get_file_line(Used_ele_of_month_filepath_of_build_2)} of
            {{ok, DataLine1}, {ok, DataLine2}} ->
                {ok, DataLine1 ++ DataLine2};
            {{ok, DataLine1}, _} ->
                {ok, DataLine1};
            {_, {ok, DataLine2}} ->
                {ok, DataLine2};
            _ ->
                {error, null}
        end,
    case Result of
        {ok, DataLine} when length(DataLine) =< 12 ->
            list_to_binary(string:join(lists:usort(DataLine), ?NL));
        {ok, DataLine} ->
	    list_to_binary(string:join(lists:sublist(lists:reverse(lists:usort(DataLine)), 12), ?NL));
        _ ->
            <<?NULL>>
    end.
    
get_file_line(Filepath) ->
    case file:read_file(Filepath) of
        {ok, Binary} ->
            {ok, string:tokens(binary_to_list(Binary), ?NL)};
        {error, Reason} ->
            {error, Reason}
    end.    

%%------------------------------------------------------------------------------------------------
%% 楼宇的分项能耗
%%------------------------------------------------------------------------------------------------
get_building_subentry_used_ele_of_month(Building_id, Year_and_month_str) ->
    [Year_str, Month_str] = string:tokens(Year_and_month_str, "-"),
    Year = list_to_integer(Year_str),
    Subentry_used_ele_of_month_filepath_of_build = 
        analyze_util:get_subentry_used_ele_of_month_filepath_of_build(Year, Building_id),
    case file:read_file(Subentry_used_ele_of_month_filepath_of_build) of
        {ok, Binary} ->
            DataList = string:tokens(binary_to_list(Binary), ?NL),
            list_to_binary(get_subentry_used_ele(DataList, Year_and_month_str));
        {error, Reason} ->
            <<?NULL>>
    end.

get_subentry_used_ele([H | T], Year_and_month_str) ->
    case lists:prefix(Year_and_month_str, H) of
        true ->
            H;
        false ->
            get_subentry_used_ele(T, Year_and_month_str)
    end;
get_subentry_used_ele([], _) ->
    ?NULL.

    

%%------------------------------------------------------------------------------------------------
%% 楼宇的月用电量
%%------------------------------------------------------------------------------------------------
get_building_used_ele_of_month(Building_id, Meter_type, Year_str) ->
    Year = list_to_integer(Year_str),
    case get_used_ele_list_of_month(Building_id, Meter_type, Year) of
        {ok, Data_line_list} ->
            Sorted_date_line_list = lists:sort(Data_line_list),
            Init_list = [{?HELP:year_and_month_str(Year, Month), " "} || Month <- lists:seq(1, ?MAXIMONTH)],
            Init_map = maps:from_list(Init_list),
            Fun = fun
                (Data_line, Map) ->
                    case string:tokens(Data_line, ?FS) of
                        [Year_month_str, Used_ele_str] ->
                            case maps:is_key(Year_month_str, Map) of
                                true -> 
                                    New_map = maps:update(Year_month_str, Used_ele_str, Map),
                                    New_map;
                                false ->
                                    Map 
                            end;
                        _ ->
                            Map
                    end
            end,
            Map_tmp = lists:foldl(Fun, Init_map, Data_line_list),
            Values = maps:values(Map_tmp),
            Values_str = string:join(Values, ","),
            list_to_binary(Values_str);
        {error, Reason} ->
            ?ERROR("get_used_ele_list(~p) is error:~p", [Building_id, Reason]),
            <<?NULL>>
    end.

get_used_ele_list_of_month(Building_id, Meter_type, Year) ->
    Every_month_used_ele_filepath_of_build = get_every_month_used_ele_filepath_of_build(Year, Building_id, Meter_type), 
    case file:read_file(Every_month_used_ele_filepath_of_build) of
        {ok, Data_bin} ->
            Data_bitstring = binary_to_list(Data_bin),
            Data_line_list = string:tokens(Data_bitstring, ?NL),
            {ok, lists:usort(Data_line_list)};
        {error, Reason} -> {error, Reason}
    end.  

get_every_month_used_ele_filepath_of_build(Year, Build, Meter_type) ->
    analyze_util:get_used_ele_of_month_filepath_of_build(Year, Build, Meter_type).

%%------------------------------------------------------------------------------------------------
%% 楼宇的日用电时长
%%------------------------------------------------------------------------------------------------
get_building_used_time_of_day(Building_id, ?CENTRAL_AC_TYPE = Meter_type, Year_and_month_str) ->
    [Year_str, Month_str] = string:tokens(Year_and_month_str, "-"),
    Year = list_to_integer(Year_str), 
    Month = list_to_integer(Month_str),
    case get_used_time_list_of_day(Building_id, Meter_type, Year, Month) of
        {ok, Data_line_list} ->
            Sorted_date_line_list = lists:sort(Data_line_list),
            Last_day = ?HELP:last_day_of_the_month(Year, Month),
            Init_list = [{?HELP:dateToStr({Year, Month, Day}), " "} || Day <- lists:seq(1, Last_day)],
            Init_map = maps:from_list(Init_list),
            Fun = fun
                (Data_line, Map) ->
                    case string:tokens(Data_line, ?FS) of
                        [Date_str, Low_speed_used_time_str, Medium_speed_used_time_str, High_speed_used_time_str] ->
                            case maps:is_key(Date_str, Map) of
                                true -> 
                                    Used_time_str = string:join([Low_speed_used_time_str, Medium_speed_used_time_str,
                                                                          High_speed_used_time_str], ?FS),
                                    New_map = maps:update(Date_str, Used_time_str, Map),
                                    New_map;
                                false ->
                                    Map 
                            end;
                        _ ->
                            Map
                    end
            end,
            Map_tmp = lists:foldl(Fun, Init_map, Data_line_list),
            Values = maps:values(Map_tmp),
            Values_str = string:join(Values, ","),
            list_to_binary(Values_str);
        {error, Reason} ->
            ?ERROR("get_used_ele_list(~p) is error:~p", [Building_id, Reason]),
            <<?NULL>>
    end;
get_building_used_time_of_day(Building_id, Meter_type, Year_and_month_str) ->
    [Year_str, Month_str] = string:tokens(Year_and_month_str, "-"),
    Year = list_to_integer(Year_str), 
    Month = list_to_integer(Month_str),
    case get_used_time_list_of_day(Building_id, Meter_type, Year, Month) of
        {ok, Data_line_list} ->
            Sorted_date_line_list = lists:sort(Data_line_list),
            Last_day = ?HELP:last_day_of_the_month(Year, Month),
            Init_list = [{?HELP:dateToStr({Year, Month, Day}), " "} || Day <- lists:seq(1, Last_day)],
            Init_map = maps:from_list(Init_list),
            Fun = fun
                (Data_line, Map) ->
                    case string:tokens(Data_line, ?FS) of
                        [Date_str, Used_time_level_1_num_str, Used_time_level_2_num_str, Used_time_level_3_num_str, Used_time_level_4_num_str] ->
                            case maps:is_key(Date_str, Map) of
                                true -> 
                                    Used_time_level_num_str = string:join([Used_time_level_1_num_str, Used_time_level_2_num_str, Used_time_level_3_num_str, Used_time_level_4_num_str], ?FS),
                                    New_map = maps:update(Date_str, Used_time_level_num_str, Map),
                                    New_map;
                                false ->
                                    Map 
                            end;
                        _ ->
                            Map
                    end
            end,
            Map_tmp = lists:foldl(Fun, Init_map, Data_line_list),
            Values = maps:values(Map_tmp),
            Values_str = string:join(Values, ","),
            list_to_binary(Values_str);
        {error, Reason} ->
            ?ERROR("get_used_ele_list(~p) is error:~p", [Building_id, Reason]),
            <<?NULL>>
    end.

get_used_time_list_of_day(Building_id, Meter_type, Year, Month) ->
    Every_day_used_time_filepath_of_build = get_every_day_used_time_filepath_of_build(Year, Month, Building_id, Meter_type), 
    case file:read_file(Every_day_used_time_filepath_of_build) of
        {ok, Data_bin} ->
            Data_bitstring = binary_to_list(Data_bin),
            Data_line_list = string:tokens(Data_bitstring, ?NL),
            {ok, lists:usort(Data_line_list)};
        {error, Reason} -> {error, Reason}
    end.  

get_every_day_used_time_filepath_of_build(Year, Month, Build, Meter_type) ->
    analyze_util:get_used_time_of_day_filepath_of_build(Year, Month, Build, Meter_type).

%%------------------------------------------------------------------------------------------------
%% 楼宇的月用电时长
%%------------------------------------------------------------------------------------------------
get_building_used_time_of_month(Building_id, ?CENTRAL_AC_TYPE = Meter_type, Year_str) ->
    Year = list_to_integer(Year_str), 
    case get_used_time_list_of_month(Building_id, Meter_type, Year) of
        {ok, Data_line_list} ->
            Sorted_date_line_list = lists:sort(Data_line_list),
            Init_list = [{?HELP:year_and_month_str(Year, Month), " "} || Month <- lists:seq(1, ?MAXIMONTH)],
            Init_map = maps:from_list(Init_list),
            Fun = fun
                (Data_line, Map) ->
                    case string:tokens(Data_line, ?FS) of
                        [Year_month_str, Low_speed_used_time_str, Medium_speed_used_time_str, High_speed_used_time_str] ->
                            case maps:is_key(Year_month_str, Map) of
                                true -> 
                                    Used_time_str = string:join([Low_speed_used_time_str, Medium_speed_used_time_str, High_speed_used_time_str], ?FS),
                                    New_map = maps:update(Year_month_str, Used_time_str, Map),
                                    New_map;
                                false ->
                                    Map 
                            end;
                        _ ->
                            Map
                    end
            end,
            Map_tmp = lists:foldl(Fun, Init_map, Data_line_list),
            Values = maps:values(Map_tmp),
            Values_str = string:join(Values, ","),
            list_to_binary(Values_str);
        {error, Reason} ->
            ?ERROR("get_used_ele_list(~p) is error:~p", [Building_id, Reason]),
            <<?NULL>>
    end;
get_building_used_time_of_month(Building_id, Meter_type, Year_str) ->
    Year = list_to_integer(Year_str), 
    case get_used_time_list_of_month(Building_id, Meter_type, Year) of
        {ok, Data_line_list} ->
            Sorted_date_line_list = lists:sort(Data_line_list),
            Init_list = [{?HELP:year_and_month_str(Year, Month), " "} || Month <- lists:seq(1, ?MAXIMONTH)],
            Init_map = maps:from_list(Init_list),
            Fun = fun
                (Data_line, Map) ->
                    case string:tokens(Data_line, ?FS) of
                        [Year_month_str, Used_time_level_1_num_str, Used_time_level_2_num_str, Used_time_level_3_num_str, Used_time_level_4_num_str] ->
                            case maps:is_key(Year_month_str, Map) of
                                true -> 
                                    Used_time_level_num_str = string:join([Used_time_level_1_num_str, Used_time_level_2_num_str, Used_time_level_3_num_str, Used_time_level_4_num_str], ?FS),
                                    New_map = maps:update(Year_month_str, Used_time_level_num_str, Map),
                                    New_map;
                                false ->
                                    Map 
                            end;
                        _ ->
                            Map
                    end
            end,
            Map_tmp = lists:foldl(Fun, Init_map, Data_line_list),
            Values = maps:values(Map_tmp),
            Values_str = string:join(Values, ","),
            list_to_binary(Values_str);
        {error, Reason} ->
            ?ERROR("get_used_ele_list(~p) is error:~p", [Building_id, Reason]),
            <<?NULL>>
    end.

get_used_time_list_of_month(Building_id, Meter_type, Year) ->
    Every_month_used_time_filepath_of_build = get_every_month_used_time_filepath_of_build(Year, Building_id, Meter_type), 
    case file:read_file(Every_month_used_time_filepath_of_build) of
        {ok, Data_bin} ->
            Data_bitstring = binary_to_list(Data_bin),
            Data_line_list = string:tokens(Data_bitstring, ?NL),
            {ok, lists:usort(Data_line_list)};
        {error, Reason} -> {error, Reason}
    end.  

get_every_month_used_time_filepath_of_build(Year, Build, Meter_type) ->
    analyze_util:get_used_time_of_month_filepath_of_build(Year, Build, Meter_type).


%%------------------------------------------------------------------------------------------------
%% 所属建筑的表的日用电量
%%------------------------------------------------------------------------------------------------
get_meters_used_ele_of_day(Build_id, Meter_type, Year_and_month_str) ->
    case analyze_util:get_meter_type_and_meter_list_by_build_id(Build_id, Meter_type) of
        [] -> <<?NULL>>;
        Meter_type_and_meter_list ->
            [Year_str, Month_str] = string:tokens(Year_and_month_str, "-"),
            Year = list_to_integer(Year_str), 
            Month = list_to_integer(Month_str),
            ?PRINT("Meter_type_and_meter_list:~p~n", [Meter_type_and_meter_list]),
            Fun = fun
                ({Meter_type_tmp, Meter}) ->
                    case analyze_util:get_meter_used_ele_list_of_day(Meter, Year, Month) of
                        {ok, Data_line_list}  ->
                            Used_ele_data_str = create_used_ele_data_of_day(Year, Month, Data_line_list),
                            Meter_and_data_str = string:join([Meter, Used_ele_data_str], ":"),
                            {true, Meter_and_data_str};
                        {error, Reason} ->
                            false
                    end 
            end,
            Meter_and_data_str_list = lists:filtermap(Fun, Meter_type_and_meter_list),
            Return_data_str = string:join(Meter_and_data_str_list, ?NL),
            list_to_binary(Return_data_str)
    end.

create_used_ele_data_of_day(Year, Month, Data_line_list) ->
    Sorted_date_line_list = lists:sort(Data_line_list),
    Last_day = ?HELP:last_day_of_the_month(Year, Month),
    Init_list = [{?HELP:dateToStr({Year, Month, Day}), " "} || Day <- lists:seq(1, Last_day)],
    Init_map = maps:from_list(Init_list),
    Fun = fun
        (Data_line, Map) ->
            case string:tokens(Data_line, ?FS) of
                [Date_str, Used_ele_str] ->
                    case maps:is_key(Date_str, Map) of
                        true -> 
                            New_map = maps:update(Date_str, Used_ele_str, Map),
                            New_map;
                        false ->
                            Map 
                    end;
                _ ->
                    Map
            end
    end,
    Map_tmp = lists:foldl(Fun, Init_map, Data_line_list),
    Values = maps:values(Map_tmp),
    Values_str = string:join(Values, ","),
    Values_str.

%%------------------------------------------------------------------------------------------------
%% 所属建筑的表的月用电量
%%------------------------------------------------------------------------------------------------
get_meters_used_ele_of_month(Build_id, Meter_type, Year_str) ->
    case analyze_util:get_meter_type_and_meter_list_by_build_id(Build_id, Meter_type) of
        [] -> <<?NULL>>;
        Meter_type_and_meter_list ->
            Year = list_to_integer(Year_str), 
            ?PRINT("Meter_type_and_meter_list:~p~n", [Meter_type_and_meter_list]),
            Fun = fun
                ({Meter_type, Meter}) ->
                    case get_meter_used_ele_list_of_month(Meter, Year) of
                        {ok, Data_line_list}  ->
                            Used_ele_data_str = create_used_ele_data_of_month(Year, Data_line_list),
                            Meter_and_data_str = string:join([Meter, Used_ele_data_str], ":"),
                            {true, Meter_and_data_str};
                        {error, Reason} ->
                            false
                    end 
            end,
            Meter_and_data_str_list = lists:filtermap(Fun, Meter_type_and_meter_list),
            Return_data_str = string:join(Meter_and_data_str_list, ?NL),
            list_to_binary(Return_data_str)
    end.

get_meter_used_ele_list_of_month(Meter, Year) ->
    Every_month_used_ele_filepath_of_meter = get_every_month_used_ele_filepath_of_meter(Year, Meter), 
    case file:read_file(Every_month_used_ele_filepath_of_meter) of
        {ok, Data_bin} ->
            Data_bitstring = binary_to_list(Data_bin),
            Data_line_list = string:tokens(Data_bitstring, ?NL),
            {ok, lists:usort(Data_line_list)};
        {error, Reason} -> {error, Reason}
    end.  

get_every_month_used_ele_filepath_of_meter(Year, Meter) ->
    analyze_util:get_used_ele_of_month_filepath(Year, Meter).

create_used_ele_data_of_month(Year, Data_line_list) ->
    Sorted_date_line_list = lists:sort(Data_line_list),
    Init_list = [{?HELP:year_and_month_str(Year, Month), " "} || Month <- lists:seq(1, ?MAXIMONTH)],
    Init_map = maps:from_list(Init_list),
    Fun = fun
        (Data_line, Map) ->
            case string:tokens(Data_line, ?FS) of
                [Date_str, Used_ele_str] ->
                    case maps:is_key(Date_str, Map) of
                        true -> 
                            New_map = maps:update(Date_str, Used_ele_str, Map),
                            New_map;
                        false ->
                            Map 
                    end;
                _ ->
                    Map
            end
    end,
    Map_tmp = lists:foldl(Fun, Init_map, Data_line_list),
    Values = maps:values(Map_tmp),
    Values_str = string:join(Values, ","),
    Values_str.


%%------------------------------------------------------------------------------------------------
%% 所属建筑的表的日用电时长
%%------------------------------------------------------------------------------------------------
get_meters_used_time_of_day(Build_id, Meter_type = ?CENTRAL_AC_TYPE, Year_and_month_str) ->
    case analyze_util:get_meter_type_and_meter_list_by_build_id(Build_id, Meter_type) of
        [] -> <<?NULL>>;
        Meter_type_and_meter_list ->
            [Year_str, Month_str] = string:tokens(Year_and_month_str, "-"),
            Year = list_to_integer(Year_str), 
            Month = list_to_integer(Month_str),
            ?PRINT("Meter_type_and_meter_list:~p~n", [Meter_type_and_meter_list]),
            Fun = fun
                ({Meter_type_tmp, Meter}) ->
                    case analyze_util:get_meter_three_speed_used_time_list_of_day(Meter, Year, Month) of
                        {ok, Data_line_list}  ->
                            Used_ele_data_str = create_used_time_data_of_day(Year, Month, Data_line_list),
                            Meter_and_data_str = string:join([Meter, Used_ele_data_str], ":"),
                            {true, Meter_and_data_str};
                        {error, Reason} ->
                            false
                    end 
            end,
            Meter_and_data_str_list = lists:filtermap(Fun, Meter_type_and_meter_list),
            Return_data_str = string:join(Meter_and_data_str_list, ?NL),
            list_to_binary(Return_data_str)
    end;
get_meters_used_time_of_day(Build_id, Meter_type, Year_and_month_str) ->
    case analyze_util:get_meter_type_and_meter_list_by_build_id(Build_id, Meter_type) of
        [] -> <<?NULL>>;
        Meter_type_and_meter_list ->
            [Year_str, Month_str] = string:tokens(Year_and_month_str, "-"),
            Year = list_to_integer(Year_str), 
            Month = list_to_integer(Month_str),
            ?PRINT("Meter_type_and_meter_list:~p~n", [Meter_type_and_meter_list]),
            Fun = fun
                ({Meter_type_tmp, Meter}) ->
                    case analyze_util:get_meter_used_time_list_of_day(Meter, Year, Month) of
                        {ok, Data_line_list}  ->
                            Used_ele_data_str = create_used_time_data_of_day(Year, Month, Data_line_list),
                            Meter_and_data_str = string:join([Meter, Used_ele_data_str], ":"),
                            {true, Meter_and_data_str};
                        {error, Reason} ->
                            false
                    end 
            end,
            Meter_and_data_str_list = lists:filtermap(Fun, Meter_type_and_meter_list),
            Return_data_str = string:join(Meter_and_data_str_list, ?NL),
            list_to_binary(Return_data_str)
    end.

create_used_time_data_of_day(Year, Month, Data_line_list) ->
    Sorted_date_line_list = lists:sort(Data_line_list),
    Last_day = ?HELP:last_day_of_the_month(Year, Month),
    Init_list = [{?HELP:dateToStr({Year, Month, Day}), " "} || Day <- lists:seq(1, Last_day)],
    Init_map = maps:from_list(Init_list),
    Fun = fun
        (Data_line, Map) ->
            case string:tokens(Data_line, ?FS) of
                [Date_str | Used_time_str] ->
                    case maps:is_key(Date_str, Map) of
                        true -> 
                            New_map = maps:update(Date_str, string:join(Used_time_str, ?FS), Map),
                            New_map;
                        false ->
                            Map 
                    end;
                _ ->
                    Map
            end
    end,
    Map_tmp = lists:foldl(Fun, Init_map, Data_line_list),
    Values = maps:values(Map_tmp),
    Values_str = string:join(Values, ","),
    Values_str.

%%------------------------------------------------------------------------------------------------
%% 所属建筑的表的月用电时长
%%------------------------------------------------------------------------------------------------
get_meters_used_time_of_month(Build_id, Meter_type = ?CENTRAL_AC_TYPE, Year_str) ->
    case analyze_util:get_meter_type_and_meter_list_by_build_id(Build_id, Meter_type) of
        [] -> <<?NULL>>;
        Meter_type_and_meter_list ->
            Year = list_to_integer(Year_str), 
            ?PRINT("Meter_type_and_meter_list:~p~n", [Meter_type_and_meter_list]),
            Fun = fun
                ({Meter_type, Meter}) ->
                    case analyze_util:get_meter_three_speed_used_time_list_of_month(Meter, Year) of
                        {ok, Data_line_list}  ->
                            Used_ele_data_str = create_used_time_data_of_month(Year, Data_line_list),
                            Meter_and_data_str = string:join([Meter, Used_ele_data_str], ":"),
                            {true, Meter_and_data_str};
                        {error, Reason} ->
                            false
                    end 
            end,
            Meter_and_data_str_list = lists:filtermap(Fun, Meter_type_and_meter_list),
            Return_data_str = string:join(Meter_and_data_str_list, ?NL),
            list_to_binary(Return_data_str)
    end;
get_meters_used_time_of_month(Build_id, Meter_type, Year_str) ->
    case analyze_util:get_meter_type_and_meter_list_by_build_id(Build_id, Meter_type) of
        [] -> <<?NULL>>;
        Meter_type_and_meter_list ->
            Year = list_to_integer(Year_str), 
            ?PRINT("Meter_type_and_meter_list:~p~n", [Meter_type_and_meter_list]),
            Fun = fun
                ({Meter_type, Meter}) ->
                    case analyze_util:get_meter_used_time_list_of_month(Meter, Year) of
                        {ok, Data_line_list}  ->
                            ?PRINT("Reason:~p~n", [Data_line_list]),
                            Used_ele_data_str = create_used_time_data_of_month(Year, Data_line_list),
                            Meter_and_data_str = string:join([Meter, Used_ele_data_str], ":"),
                            {true, Meter_and_data_str};
                        {error, Reason} ->
                            false
                    end 
            end,
            Meter_and_data_str_list = lists:filtermap(Fun, Meter_type_and_meter_list),
            Return_data_str = string:join(Meter_and_data_str_list, ?NL),
            list_to_binary(Return_data_str)
    end.

create_used_time_data_of_month(Year, Data_line_list) ->
    Sorted_date_line_list = lists:sort(Data_line_list),
    Init_list = [{?HELP:year_and_month_str(Year, Month), " "} || Month <- lists:seq(1, ?MAXIMONTH)],
    Init_map = maps:from_list(Init_list),
    Fun = fun
        (Data_line, Map) ->
            case string:tokens(Data_line, ?FS) of
                [Date_str | Used_time_str] ->
                    case maps:is_key(Date_str, Map) of
                        true -> 
                            New_map = maps:update(Date_str, string:join(Used_time_str, ?FS), Map),
                            New_map;
                        false ->
                            Map 
                    end;
                _ ->
                    Map
            end
    end,
    Map_tmp = lists:foldl(Fun, Init_map, Data_line_list),
    Values = maps:values(Map_tmp),
    Values_str = string:join(Values, ","),
    Values_str.

%%------------------------------------------------------------------------------------------------
%% 使用情况月度报表
%%------------------------------------------------------------------------------------------------

%% 获取当前月往前12月份列表
get_report_year_month_list(Meter_type, Meter) ->
    {Year, Month, _} = erlang:date(),
    Year_month_list = get_report_year_month_list_(Year, Month, [], 12),
    to_binary(string:join(lists:reverse(Year_month_list), ?NL)).

get_report_year_month_list_(Year, Month, Year_month_list, Month_count) when (Month_count > 0) ->
    {Year_tmp, Month_tmp} = ?HELP:get_prev_month(Year, Month),
    Year_and_month_str = ?HELP:year_and_month_str(Year_tmp, Month_tmp),
    get_report_year_month_list_(Year_tmp, Month_tmp, [Year_and_month_str | Year_month_list], Month_count-1);
get_report_year_month_list_(_, _, Year_month_list, 0) ->
    Year_month_list.

%% 由月份获取报表数据
get_report_of_month(Meter_type, Meter, Year_and_month) ->
    [Year_str, Month_str] = string:tokens(Year_and_month, "-"),
    Year = list_to_integer(Year_str), 
    Month = list_to_integer(Month_str),
    Year_and_month_str = ?HELP:year_and_month_str(Year, Month),
    case get_report_line_list(Meter, Year) of
        {ok, []} -> <<?NULL>>;
        {ok, Data_line_list} ->
            Report_of_month = get_report_of_month_(Data_line_list, Year_and_month_str),
            to_binary(Report_of_month);
        {error, Reason} ->   
            ?ERROR("get_report_line_list(~p, ~p) is error:~p", [Meter, Year, Reason]),  
            <<?NULL>>
    end.

get_report_line_list(Meter, Year) ->
    Report_of_month_filepath = analyze_util:get_used_report_of_month_filepath(Year, Meter),
    case file:read_file(Report_of_month_filepath) of
        {ok, Data_bin} ->
            Data_bitstring = binary_to_list(Data_bin),
            Data_line_list = string:tokens(Data_bitstring, ?NL),
            {ok, lists:usort(Data_line_list)};
        {error, Reason} -> {error, Reason}
    end.  

get_report_of_month_([Data_line | T], Year_and_month_str) ->
    case string:tokens(Data_line, ?FS) of
        [Year_and_month_str | _] ->
            Data_line;
        _ ->
            get_report_of_month_(T, Year_and_month_str)
    end;
get_report_of_month_([], _) ->
    <<?NULL>>.

%%------------------------------------------------------------------------------------------------
%% 设备每日用电量列表
%%------------------------------------------------------------------------------------------------
get_meter_used_ele_list_of_day(Meter_type, Meter, Year_and_month) ->
    [Year_str, Month_str] = string:tokens(Year_and_month, "-"),
    Year = list_to_integer(Year_str), 
    Month = list_to_integer(Month_str),
    case analyze_util:get_meter_used_ele_list_of_day(Meter, Year, Month) of
        {ok, Data_line_list}  ->
            to_binary(string:join(lists:usort(Data_line_list), ?NL));
        {error, Reason} ->
            ?ERROR("get_meter_used_ele_list_of_day(~p, ~p, ~p) is error:~p", [Meter, Year, Month, Reason]),  
            <<?NULL>>
    end.

%%------------------------------------------------------------------------------------------------
%% 设备每日用电时长列表
%%------------------------------------------------------------------------------------------------
get_meter_used_time_list_of_day(Meter_type, Meter, Year_and_month) ->
    [Year_str, Month_str] = string:tokens(Year_and_month, "-"),
    Year = list_to_integer(Year_str), 
    Month = list_to_integer(Month_str),
    case analyze_util:get_meter_used_time_list_of_day(Meter, Year, Month) of
        {ok, Data_line_list}  ->
            to_binary(string:join(lists:usort(Data_line_list), ?NL));
        {error, Reason} ->
            ?ERROR("get_meter_used_time_list_of_day(~p, ~p, ~p) is error:~p", [Meter, Year, Month, Reason]),  
            <<?NULL>>
    end.

%%-------------------------------------------------------------------------------------------------
%% 房间课程表
%%-------------------------------------------------------------------------------------------------
get_time_tables(Room_id) ->
    case analyze_build_time_table_store:lookup_by_room_id(Room_id) of
        {ok, Time_table_list} ->
            to_binary(string:join([get_time_table_str(Time_table) || Time_table <- Time_table_list], ?NL));
        {error, _} ->
            <<?NULL>>
    end.

get_time_table_str(Time_table) ->
    #time_table{
            type_and_labels_of_meter = Type_and_labels_of_meter,
            level = Level,
            validity_date_start = Validity_date_start,
            validity_date_end = Validity_date_end,
            holiday_mode = Holiday_mode,
            time_list = Time_list
    } = Time_table,
    Type_and_labels_of_meter_str = string:join(Type_and_labels_of_meter, "&"),
    Level_str = integer_to_list(Level),
    Validity_date_start_str = ?HELP:dateToStr(Validity_date_start),
    Validity_date_end_str = ?HELP:dateToStr(Validity_date_end),
    Holiday_mode_str = integer_to_list(Holiday_mode),
    Time_list_str = get_time_list_str(Time_list),
    string:join([Type_and_labels_of_meter_str, Level_str, Validity_date_start_str, Validity_date_end_str, Holiday_mode_str, Time_list_str], "#").

get_time_list_str(Time_list) ->
    Fun = fun
        (Time = {Day_of_the_week, Time_list_of_day}) ->
            Day_of_the_week_str = integer_to_list(Day_of_the_week),
            Time_list_of_day_str = string:join(lists:map(fun
                ({{Hour_1, Minute_1}, {Hour_2, Minute_2}})->
                    Time_str_1 = get_hour_minute_str(Hour_1, Minute_1),
                    Time_str_2 = get_hour_minute_str(Hour_2, Minute_2),
                    string:join([Time_str_1, Time_str_2], "-")
            end, Time_list_of_day), " "),
            string:join([Day_of_the_week_str, Time_list_of_day_str], "&")
    end,
    string:join(lists:map(Fun, Time_list), ";").

get_hour_minute_str(Hour, Minute) when is_integer(Hour) andalso is_integer(Minute) ->
    string:join([integer_to_list(Hour), integer_to_list(Minute)], ":");
get_hour_minute_str(_, _) ->
    erlang:error(badarg).


%% 获取建筑下设备的状态字
get_status_word(Meter_and_type_list) ->
    get_status_word_(Meter_and_type_list, []).

get_status_word_([{MeterType, Meter} | T], List) ->
    case analyze_meter_util:get_meter_blob(MeterType, Meter) of
        {ok, Blob} ->
            case analyze_meter_blob_util:get_status_word(Blob) of
                undefined ->
                    get_status_word_(T, List);
                StatusWordBin ->
                    Item = string:join([Meter, MeterType, binary_to_list(StatusWordBin)], ","),
                    get_status_word_(T, [Item | List])
            end;
        _ ->
            get_status_word_(T, List)
    end;
get_status_word_([], List) ->
    List.

%% 根据建筑ID获取状态字            
get_status_word_by_building_id(BuildingId) ->
    case analyze_build_meter:lookup(BuildingId) of
        {error, _} ->
            <<?NULL>>;
        {ok, Meter_type_and_meter_list} ->
            Meter_type_and_meter_and_status_word_list = get_status_word(Meter_type_and_meter_list),
            case Meter_type_and_meter_and_status_word_list of
                [] ->
                    <<?NULL>>;
                _ ->
                    ?PRINT("~p~n", [Meter_type_and_meter_and_status_word_list]),
                    Result = string:join(Meter_type_and_meter_and_status_word_list, ";"),
                    list_to_binary(Result)
            end
    end.                                               

%% 根据建筑ID获取继电器状态及通讯质量
get_relay_status_and_cq_by_building_id(BuildingId) ->
    get_relay_status_and_cq_by_building_id(BuildingId, undefined).

get_relay_status_and_cq_by_building_id(BuildingId, MeterType) ->
    case analyze_build_meter:lookup(BuildingId) of
        {error, _} ->
            <<?NULL>>;
        {ok, Meter_type_and_meter_list_tmp} ->
            Meter_type_and_meter_list = 
                case MeterType of
                    undefined ->
                        Meter_type_and_meter_list_tmp;
                    _ when is_list(MeterType) ->
                        [Meter_type_and_meter || Meter_type_and_meter = {Type, _} <- Meter_type_and_meter_list_tmp, 
                                                 Type =:= MeterType]
                end,
            Meter_type_and_meter_and_relay_status_and_cq_list = get_relay_status_and_cq(Meter_type_and_meter_list),
            case Meter_type_and_meter_and_relay_status_and_cq_list of
                [] ->
                    <<?NULL>>;
                _ ->
                    ?PRINT("~p~n", [Meter_type_and_meter_and_relay_status_and_cq_list]),
                    Result = string:join(Meter_type_and_meter_and_relay_status_and_cq_list, ";"),
                    list_to_binary(Result)
            end
    end.                                               
            

%% 获取建筑下设备的继电器状态及通信质量
get_relay_status_and_cq(Meter_and_type_list) ->
    get_relay_status_and_cq_(Meter_and_type_list, []).

get_relay_status_and_cq_([{MeterType, Meter} | T], List) ->
    case analyze_meter_util:get_cq(MeterType, Meter) of
        {ok, Cq} ->            
            case analyze_meter_on_off_status:lookup(MeterType, Meter) of
                {ok, OnOffStatus} ->
                    Item = string:join([Meter, MeterType, integer_to_list(OnOffStatus), integer_to_list(Cq)], ","),
                    get_relay_status_and_cq_(T, [Item | List]);
                _ ->
                    get_relay_status_and_cq_(T, List)
            end;
        _ ->
            get_relay_status_and_cq_(T, List)
    end;
get_relay_status_and_cq_([], List) ->
    List.

%% 楼宇设备个数及在线个数
get_device_count_and_online_num_of_building(BuildingIds) ->
    get_device_count_and_online_num_of_building(BuildingIds, undefined).

get_device_count_and_online_num_of_building(BuildingIds, OldMeterType) ->
    BuildingIdList = string:tokens(BuildingIds, ","),
    Fun = 
        fun(BuildingId) ->
                case analyze_util:get_meter_type_and_meter_list_by_build_id(BuildingId, OldMeterType) of
                    [] ->
                        false;
                    MeterTypeAndMeterList ->
                        Fun = 
                            fun({MeterType, Meter}, {Count, OnLineNum}) ->
                                    case analyze_meter_util:get_cq(MeterType, Meter) of
                                        {ok, Cq} when (Cq >= 60) ->
                                            {Count+1, OnLineNum+1};
                                        Other ->
                                            {Count+1, OnLineNum}
                                    end
                            end,
                        {Count, OnLineNum} = lists:foldl(Fun, {0, 0}, MeterTypeAndMeterList),
                        {true, string:join([BuildingId, integer_to_list(Count), integer_to_list(OnLineNum)], ",")}
                end
        end,
    string:join(lists:filtermap(Fun, BuildingIdList), ";").                    






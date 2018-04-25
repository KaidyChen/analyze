-module (analyze_util).

-include("analyze.hrl").
-include("analyze_config.hrl").
-include("analyze_meter.hrl").
-include("print.hrl").

-compile([export_all]).

%%%=====================================================================
%%% 杂函数
%%%=====================================================================

%% 获取所有的休息日列表
get_holiday_list() ->
    case file:read_file(?HOLIDAY_CONF) of
        {ok, Data_bin} ->
            Data_list = binary_to_list(Data_bin),
            Holiday_list = string:tokens(Data_list, ?NL),
            CharList = [$\r, $ ],
            Fun = 
                fun(DataLine) ->
                        ?HELP:strToDate(?HELP:strip(CharList, DataLine))
                end,
            {ok, lists:map(Fun, Holiday_list)};
        {error, Reason} ->
            {error, Reason}
    end.

is_holiday(Date_str) when is_list(Date_str) ->
    is_holiday(?HELP:strToDate(Date_str));
is_holiday(Date) when is_tuple(Date) ->
    {ok, HolidayList} = app_util:env(holidays),
    lists:member(Date, HolidayList).

%% 获取设备上报间隔
get_report_interval(Meter_type) ->
    {ok, ReportIntervalList} = app_util:env(report_intervals),
    proplists:get_value(Meter_type, ReportIntervalList, ?DEFAULT_REPORT_INTERVAL).

%%%=====================================================================
%%% 批量表的条件字段解析
%%%=====================================================================
get_meter_field_list_by_condition(Condition_field, Meter_field_list) ->
    Fun = 
        fun(Meter_field, New_Meter_field_list_tmp) ->
                {Eqpt_type_tmp, Meter_tmp} = analyze_meter_field:get_meter_type_and_meter_by_meter_field(Meter_field),
                Building_tmp = analyze_meter_field:get_build_id_by_meter_field(Meter_field),
                case is_match_condition(Condition_field, {Building_tmp, Eqpt_type_tmp, Meter_tmp}) of
                    true ->
                        [Meter_field | New_Meter_field_list_tmp];
                    false ->
                        New_Meter_field_list_tmp
                end
        end,
    lists:foldl(Fun, [], Meter_field_list).

%% 解析Crontab/Task condition
parse_condition(Condition_Str) ->
    Condition_List = string:tokens(Condition_Str, "&="),
    Building = get_args(Condition_List, "building"),
    Meter = get_args(Condition_List, "meter"),
    Eqpt_type = get_args(Condition_List, "eqpt_type"),
    {Building, Meter, Eqpt_type}.

get_args(_Condition_List = [Item, Value | _New_List], Item) ->
    Value;
get_args(_Condition_List = [_Item, _Value | New_List], Item) ->
    get_args(New_List, Item);
get_args([], _Item) ->
    "*".

is_match_condition(Condition_field, Field) ->
    {Building_condition, Meter_condition, Eqpt_type_condition} = Condition_field,
    {Building_tmp, Eqpt_type_tmp, Meter_tmp} = Field,
    is_match_meter(Meter_condition, Meter_tmp) andalso is_match_eqpt_type(Eqpt_type_condition, Eqpt_type_tmp) andalso is_match_building(Building_condition, Building_tmp). 

is_match_building(Building_condition, Building_tmp) ->
    %?DEBUG("~p ~p~n", [Building_condition, Building_tmp]),
    if
        Building_condition =:= "*" ->
            true;
        true ->
            try re:run(Building_tmp, Building_condition, [ungreedy]) of
                {match, _} ->
                    true;
                nomatch ->
                    false
            catch
                _Class:_Reason ->
                    false
            end
    end.

is_match_eqpt_type(Eqpt_type_condition, Eqpt_type_tmp) ->
    if
        (Eqpt_type_condition =:= "*") orelse (Eqpt_type_condition =:= Eqpt_type_tmp) ->
            true;
        true -> 
            false 
    end.

is_match_meter(Meter_condition, Meter_tmp) ->
    if
        Meter_condition =:= "*" ->
            true;
        true ->
            try re:run(Meter_tmp, Meter_condition, [ungreedy]) of
                {match, _} ->
                    true;
                nomatch ->
                    false
            catch
                _Class:_Reason ->
                    false
            end
    end.

%%%=====================================================================
%%% end
%%%=====================================================================


%%%=====================================================================
%%% 所属楼宇的表信息
%%%=====================================================================

%% 获取楼宇的所有某一类型设备

get_meter_type_and_meter_list_by_build_id(Build_id, Meter_type) ->
    Build_id_and_meter_type_and_meter_list = case length(Build_id) of
        ?ROOM_ID_LEN ->
            get_all_meter_of_room_by_meter_type(Meter_type);
        ?FLOOR_ID_LEN ->
            get_all_meter_of_floor_by_meter_type(Meter_type);
        ?BUILDING_ID_LEN ->
            get_all_meter_of_building_by_meter_type(Meter_type);
        ?GARDEN_ID_LEN ->
            get_all_meter_of_garden_by_meter_type(Meter_type);
        _ ->
            []
    end,
    case lists:keyfind(Build_id, 1, Build_id_and_meter_type_and_meter_list) of
        {Build_id, Meter_type_and_meter_list} ->
            Meter_type_and_meter_list;
        false ->
            []
    end.

get_all_meter_of_room_by_meter_type(Meter_type) ->
    get_all_meter_of_build_by_meter_type(Meter_type, ?ROOM).

get_all_meter_of_floor_by_meter_type(Meter_type) ->
    get_all_meter_of_build_by_meter_type(Meter_type, ?FLOOR).

get_all_meter_of_building_by_meter_type(Meter_type) ->
    get_all_meter_of_build_by_meter_type(Meter_type, ?BUILDING).

get_all_meter_of_garden_by_meter_type(Meter_type) ->
    get_all_meter_of_build_by_meter_type(Meter_type, ?GARDEN).

%% 获取楼宇的对应设备
get_all_meter_of_build_by_meter_type(SelectMeter_type, Build_type) ->
    case get_room_and_meter_list() of
        [] -> [];
        Room_and_meter_info_list ->
            Fun = 
                fun({Build_id, Meter_field_list}, Map) ->
                        New_build_id = get_build_id(Build_id, Build_type),
                        Fun1 = 
                            fun({Meter_type, Meter}) when Meter_type =:= SelectMeter_type ->
                                    true;
                               (_) ->
                                    false                                    
                            end,
                        Meter_type_and_meter_list = lists:filter(Fun1, Meter_field_list),
                    case Meter_type_and_meter_list of
                        [] -> Map;
                        _ ->
                            case maps:is_key(New_build_id, Map) of
                                false -> 
                                    maps:put(New_build_id, Meter_type_and_meter_list, Map);
                                true ->
                                    New_value = lists:merge(maps:get(New_build_id, Map), Meter_type_and_meter_list),
                                    maps:put(New_build_id, New_value, Map)
                            end  
                    end
            end,
            Meter_type_and_meter_map = lists:foldl(Fun, maps:new(), Room_and_meter_info_list),
            maps:to_list(Meter_type_and_meter_map)
    end.

get_type_and_meter_and_master_label_of_building_by_meter_type_list(Meter_type_list) ->
    get_type_and_meter_and_master_label_of_build_by_meter_type_list(Meter_type_list, ?BUILDING).

get_type_and_meter_and_master_label_of_build_by_meter_type_list(Meter_type_list, Build_type) ->
    case get_room_and_meter_list() of
        [] -> [];
        Room_and_meter_info_list ->
            Fun = 
                fun({Build_id, Meter_type_and_meter_list}, Map) ->
                        New_build_id = get_build_id(Build_id, Build_type),
                        Fun1 = 
                            fun({Meter_type, Meter}) ->
                                    case analyze_meter_field_store:lookup(Meter_type, Meter) of
                                        {ok, Meter_field} ->                                            
                                            Master_label = analyze_meter_field:get_master_label_by_meter_field(Meter_field),
                                            case lists:member(Meter_type, Meter_type_list) of
                                                true ->
                                                    {true, {Meter_type, Meter, Master_label}};
                                                _ ->
                                                    false
                                            end;
                                        _ ->
                                            false
                                    end
                            end,
                        Meter_type_and_meter_and_master_label_list = lists:filtermap(Fun1, Meter_type_and_meter_list),
                        case Meter_type_and_meter_and_master_label_list of
                            [] -> Map;
                            _ ->
                                case maps:is_key(New_build_id, Map) of
                                    false -> 
                                        maps:put(New_build_id, Meter_type_and_meter_and_master_label_list, Map);
                                    true ->
                                        New_value = lists:merge(maps:get(New_build_id, Map), 
                                                                Meter_type_and_meter_and_master_label_list),
                                        maps:put(New_build_id, New_value, Map)
                                end  
                        end
                end,
            Meter_type_and_meter_map = lists:foldl(Fun, maps:new(), Room_and_meter_info_list),
            maps:to_list(Meter_type_and_meter_map)
    end.
    

get_build_id(Build_id, ?ROOM) ->
    Build_id;
get_build_id(Build_id, ?FLOOR) ->
    lists:sublist(Build_id, ?FLOOR_ID_LEN);
get_build_id(Build_id, ?BUILDING) ->
    lists:sublist(Build_id, ?BUILDING_ID_LEN);
get_build_id(Build_id, ?GARDEN) ->
    lists:sublist(Build_id, ?GARDEN_ID_LEN).

get_room_and_meter_list() ->
    analyze_build_meter:tab2list().

%%%=====================================================================
%%% end
%%%=====================================================================

%%%=====================================================================
%%% 日/月用量，使用时长相关的接口函数
%%%=====================================================================

%% 建筑的日用电量文件路径
get_used_ele_of_day_filepath_of_build(_Date = {Year, Month, _Day}, Build, Meter_type) ->
    get_used_ele_of_day_filepath_of_build(Year, Month, Build, Meter_type).

get_used_ele_of_day_filepath_of_build(Year, Month, Build, Meter_type) ->
    {Meter_type, Meter_type_dirname} = lists:keyfind(Meter_type, 1, ?METER_TYPE_TO_DIRNAME),
    Next_dir = lists:concat([?USED_ELE_OF_DAY_DIR, "/", Meter_type_dirname]),
    ?HELP:get_year_month_file_path_of_build(Year, Month, Build, Next_dir).

%% 建筑的月用电量文件路径
get_used_ele_of_month_filepath_of_build(Year, Build, Meter_type) ->
    {Meter_type, Meter_type_dirname} = lists:keyfind(Meter_type, 1, ?METER_TYPE_TO_DIRNAME),
    Next_dir = lists:concat([?USED_ELE_OF_MONTH_DIR, "/", Meter_type_dirname]),
    ?HELP:get_year_file_path_of_build(Year, Build, Next_dir).

get_used_ele_of_month_filepath_of_build(Year, Build) ->
    Next_dir = ?USED_ELE_OF_MONTH_DIR,
    ?HELP:get_year_file_path_of_build(Year, Build, Next_dir).

get_subentry_used_ele_of_month_filepath_of_build(Year, Build) ->
    Next_dir = ?SUBENTRY_USED_ELE_OF_MONTH_DIR,
    ?HELP:get_year_file_path_of_build(Year, Build, Next_dir).
    

%% 建筑的日用电时长文件路径
get_used_time_of_day_filepath_of_build(_Date = {Year, Month, _Day}, Build, Meter_type) ->
    get_used_time_of_day_filepath_of_build(Year, Month, Build, Meter_type).

get_used_time_of_day_filepath_of_build(Year, Month, Build, Meter_type) ->
    {Meter_type, Meter_type_dirname} = lists:keyfind(Meter_type, 1, ?METER_TYPE_TO_DIRNAME),
    Next_dir = lists:concat([?USED_TIME_OF_DAY_DIR, "/", Meter_type_dirname]),
    ?HELP:get_year_month_file_path_of_build(Year, Month, Build, Next_dir).

%% 建筑的月用电时长文件路径
get_used_time_of_month_filepath_of_build(Year, Build, Meter_type) ->
    {Meter_type, Meter_type_dirname} = lists:keyfind(Meter_type, 1, ?METER_TYPE_TO_DIRNAME),
    Next_dir = lists:concat([?USED_TIME_OF_MONTH_DIR, "/", Meter_type_dirname]),
    ?HELP:get_year_file_path_of_build(Year, Build, Next_dir).

%% 获取表保存日用电量的文件路径
get_used_ele_of_day_filepath(_Date = {Year, Month, _Day}, Meter) ->
    get_used_ele_of_day_filepath(Year, Month, Meter).       

get_used_ele_of_day_filepath(Year, Month, Meter) ->
    ?HELP:get_year_month_file_path(Year, Month, Meter, ?USED_ELE_OF_DAY_DIR).

%% 获取保存月用电量的文件路径
get_used_ele_of_month_filepath(Year, Meter) ->
    ?HELP:get_year_file_path(Year, Meter, ?USED_ELE_OF_MONTH_DIR).

%% 获取保存月工作日/非工作日用电量的文件路径
get_used_ele_of_month_work_and_nonwork_filepath(Year, Meter) ->
    ?HELP:get_year_file_path(Year, Meter, ?WORK_AND_NONWORK_USED_ELE_OF_MONTH_DIR).

get_used_time_of_month_work_and_nonwork_filepath(Year, Meter) ->
    ?HELP:get_year_file_path(Year, Meter, ?WORK_AND_NONWORK_USED_TIME_OF_MONTH_DIR).

%% 获取表保存工作日工作时段/非工作时段用电量的文件路径
get_used_ele_of_workday_filepath(_Date = {Year, Month, _Day}, Meter) ->
    get_used_ele_of_workday_filepath(Year, Month, Meter).

get_used_ele_of_workday_filepath(Year, Month, Meter) ->
    ?HELP:get_year_month_file_path(Year, Month, Meter, ?USED_ELE_OF_WORKDAY_DIR).

%% 获取保存月工作日工作时段/非工作时段用电量的文件路径
get_used_ele_of_workday_month_filepath(Year, Meter) ->
    ?HELP:get_year_file_path(Year, Meter, ?USED_ELE_OF_WORKDAY_MONTH_DIR).

%% 获取表保存工作日工作时段/非工作时段用电时长的文件路径
get_used_time_of_workday_filepath(_Date = {Year, Month, _Day}, Meter) ->
    get_used_time_of_workday_filepath(Year, Month, Meter).

get_used_time_of_workday_filepath(Year, Month, Meter) ->
    ?HELP:get_year_month_file_path(Year, Month, Meter, ?USED_TIME_OF_WORKDAY_DIR).

%% 获取保存月工作日工作时段/非工作时段用电时长的文件路径
get_used_time_of_workday_month_filepath(Year, Meter) ->
    ?HELP:get_year_file_path(Year, Meter, ?USED_TIME_OF_WORKDAY_MONTH_DIR).
    
%% 获取表保存日用电时长的文件路径
get_used_time_of_day_filepath(_Date = {Year, Month, _Day}, Meter) ->
    get_used_time_of_day_filepath(Year, Month, Meter).

get_used_time_of_day_filepath(Year, Month, Meter) ->
    ?HELP:get_year_month_file_path(Year, Month, Meter, ?USED_TIME_OF_DAY_DIR).   

%% 中央空调中高低三档日用电时长
get_three_speed_used_time_of_day_filepath(_Date = {Year, Month, _Day}, Meter) ->
    get_three_speed_used_time_of_day_filepath(Year, Month, Meter).

get_three_speed_used_time_of_day_filepath(Year, Month, Meter) ->
    ?HELP:get_year_month_file_path(Year, Month, Meter, ?THREE_SPEED_USED_TIME_OF_DAY_DIR).

%% 中央空调中高低三档月用电时长
get_three_speed_used_time_of_month_filepath(Year, Meter) ->
    ?HELP:get_year_file_path(Year, Meter, ?THREE_SPEED_USED_TIME_OF_MONTH_DIR).

%% 年月每一天的用电时长的文件路径
get_every_day_used_time_filepath(Year, Month, Meter) ->
    ?HELP:get_year_month_file_path(Year, Month, Meter, ?USED_TIME_OF_DAY_DIR).

%% 获取保存月使用时长的文件路径
get_used_time_of_month_filepath(Year, Meter) ->
    ?HELP:get_year_file_path(Year, Meter, ?USED_TIME_OF_MONTH_DIR).

%% 月冻结数据的文件路径
get_frozen_data_of_month_filepath(Year, Meter) ->
    ?HELP:get_year_file_path(Year, Meter, ?FROZEN_DATA_OF_MONTH).

%% 月使用数据
get_used_data_of_month_filepath(Year, Meter) ->
    ?HELP:get_year_file_path(Year, Meter, ?USED_DATA_OF_MONTH).

%% 每日通宵使用时长的文件路径
get_used_time_all_night_of_day_filepath(_Date = {Year, Month, _Day}, Meter) ->
    get_used_time_all_night_of_day_filepath(Year, Month, Meter).

get_used_time_all_night_of_day_filepath(Year, Month, Meter) ->
    ?HELP:get_year_month_file_path(Year, Month, Meter, ?USED_TIME_ALL_NIGHT_OF_DAY_DIR). 

%% 年月每一天的通宵用电时长的文件路径
get_every_day_all_night_used_time_filepath(Year, Month, Meter) ->
    ?HELP:get_year_month_file_path(Year, Month, Meter, ?USED_TIME_ALL_NIGHT_OF_DAY_DIR).

%% 获取保存月通宵使用时长的文件路径
get_used_time_all_night_of_month_filepath(Year, Meter) ->
    ?HELP:get_year_file_path(Year, Meter, ?USED_TIME_ALL_NIGHT_OF_MONTH_DIR).

%% 获取年月所有日期的日用电量列表
get_meter_used_ele_list_of_day(Meter, Year, Month) ->
    Every_day_used_ele_filepath_of_meter = get_used_ele_of_day_filepath(Year, Month, Meter), 
    get_data_line_list(Every_day_used_ele_filepath_of_meter).

%% 获取年月所有日期的日用电时长列表
get_meter_used_time_list_of_day(Meter, Year, Month) ->
    Every_day_used_time_filepath_of_meter = get_used_time_of_day_filepath(Year, Month, Meter), 
    get_data_line_list(Every_day_used_time_filepath_of_meter).

%% 中央空调年月所有日期的日用电时长列表（三档时速）
get_meter_three_speed_used_time_list_of_day(Meter, Year, Month) ->
    Every_day_used_time_filepath_of_meter = get_three_speed_used_time_of_day_filepath(Year, Month, Meter), 
    get_data_line_list(Every_day_used_time_filepath_of_meter).
    
%% 获取年所有月用电时长列表
get_meter_used_time_list_of_month(Meter, Year) ->
    Every_month_used_time_filepath_of_meter = get_used_time_of_month_filepath(Year, Meter), 
    get_data_line_list(Every_month_used_time_filepath_of_meter).

get_meter_three_speed_used_time_list_of_month(Meter, Year) ->
    Every_month_used_time_filepath_of_meter = get_three_speed_used_time_of_month_filepath(Year, Meter), 
    get_data_line_list(Every_month_used_time_filepath_of_meter).

%% 获取保存空调日可节省电量的文件路径
get_conserve_ele_of_day_filepath(_Date = {Year, Month, _Day}, Meter) ->
    get_conserve_ele_of_day_filepath(Year, Month, Meter).

get_conserve_ele_of_day_filepath(Year, Month, Meter) ->
    ?HELP:get_year_month_file_path(Year, Month, Meter, ?CONSERVE_ELE_OF_DAY_DIR).

get_conserve_ele_of_month_filepath(Year, Meter) ->
    ?HELP:get_year_file_path(Year, Meter, ?CONSERVE_ELE_OF_MONTH_DIR).

%% 读取数据行
get_data_line_list(Filepath) ->
    case file:read_file(Filepath) of
        {ok, Data_bin} ->
            Data_bitstring = binary_to_list(Data_bin),
            Data_line_list = string:tokens(Data_bitstring, ?NL),
            {ok, lists:usort(Data_line_list)};
        {error, Reason} -> {error, Reason}
    end.  

%%%=====================================================================
%%% end
%%%=====================================================================

%%%=====================================================================
%%% 月度报表
%%%=====================================================================

%% 月使用电量情况报表
get_used_report_of_month_filepath(Year, Meter) ->
    ?HELP:get_year_file_path(Year, Meter, ?USED_REPORT_OF_MONTH_DIR).


%%%=====================================================================
%%% 上报率
%%%=====================================================================

%% 获取数据完整率的文件路径
get_integrity_rate_filepath(Year, Month, Meter) ->
    ?HELP:get_year_month_file_path(Year, Month, Meter, ?INTEGRITY_RATE_DIR).

%% 每日平均上报完整率的文件路径
get_integrity_rate_of_day_filepath(_Date = {Year, Month, Day}, Meter) ->
    get_integrity_rate_of_day_filepath(Year, Month, Meter).

get_integrity_rate_of_day_filepath(Year, Month, Meter) ->
    ?HELP:get_year_month_file_path(Year, Month, Meter, ?INTEGRITY_RATE_OF_DAY_DIR).

get_integrity_rate_of_month_filepath(Year, Meter) ->
    ?HELP:get_year_file_path(Year, Meter, ?INTEGRITY_RATE_OF_MONTH_DIR).

%%%=====================================================================
%%% end
%%%=====================================================================


%%%=====================================================================
%%% 上报率
%%%=====================================================================

%% 获取通讯完整率的文件路径
get_commu_rate_of_month_filepath(Year, Meter) ->
    ?HELP:get_year_file_path(Year, Meter, ?USED_COMMU_OF_MONTH_DIR).

%%%=====================================================================
%%% end
%%%=====================================================================


%%%=====================================================================
%%% 排名
%%%=====================================================================

get_ranking_filepath(_Date = {Year, Month, _Day}) ->
    get_ranking_filepath(Year, Month).

get_ranking_filepath(Year, Month) ->
    Year_and_month_str = ?HELP:year_and_month_str(Year, Month),
    SaveFileName = lists:concat([Year_and_month_str, ?SUFFIX]),
    filename:join([?RANGKING_DIR, SaveFileName]).

get_ranking_list() ->
    Date = ?HELP:date(),
    Filepath = get_ranking_filepath(Date),
    get_ranking_list_(Filepath).

get_ranking_list(Year, Month) ->
    Filepath = get_ranking_filepath(Year, Month),
    get_ranking_list_(Filepath).

get_ranking_list_(Filepath) ->
    case file:read_file(Filepath) of
        {ok, Data_bin} ->
            Data_bitstring = binary_to_list(Data_bin),
            Data_line_list = string:tokens(Data_bitstring, ?NL),
            {ok, Data_line_list};
        {error, Reason} -> {error, Reason}
    end.

%%%=====================================================================
%%% end
%%%=====================================================================

get_chinese_meter_type(?AC_TYPE) ->
    "分体空调";
get_chinese_meter_type(?SOCKET_TYPE) ->
    "插座表";
get_chinese_meter_type(?FOUR_WAY_SWITCH_TYPE) ->
    "灯控面板";
get_chinese_meter_type(?CENTRAL_AC_TYPE) ->
    "中央空调";
get_chinese_meter_type(_) ->
    "未知设备类型".







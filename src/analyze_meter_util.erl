-module(analyze_meter_util).

-include("analyze.hrl").
-include("analyze_config.hrl").
-include("cmd_obj.hrl").
-include("print.hrl").

-compile([export_all]).


%%%=====================================================================
%%% 调用analyze_meter_to_pid模块函数
%%%=====================================================================

%% 获取所有的表类型和表号
get_all_meter_type_and_meter() ->
    analyze_meter_pid:get_all_meter_type_and_meter().

select_meter_by_meter_type(Meter_type) ->
    analyze_meter_pid:select_meter_by_meter_type(Meter_type).

%% 判断虚拟表meter进程是否正在运行
%% Meter:表号
%% get_running_pid(Meter_type, Meter) -> {ok, pid()} | {error, Reason}
get_running_pid(Meter_type, Meter) ->
    analyze_meter_pid:get_running_pid(Meter_type, Meter).

%% 根据Pid查找Meter_type, Meter
lookup_by_pid(Pid) ->
    analyze_meter_pid:lookup_by_pid(Pid).

%%%=====================================================================
%%% end
%%%=====================================================================


%%--------------------------------------------------------------------------
%% 触发式任务
%%--------------------------------------------------------------------------

%% 根据表类型以及上报的消息类型查找任务列表
lookup_tasks(Meter_type, Msg_type)->
    case analyze_datamsg_to_tasks:lookup({Meter_type, Msg_type}) of
        {ok, Tasks_List} ->
            {ok, Tasks_List};
        _Other ->
            {error, not_found}
    end.

%% 主动上报到来，触发计算任务
run_tasks(Meter_type, Meter, Msg_type, Meter_blob) ->
    analyze_trigger_task:run_tasks(Meter_type, Meter, Msg_type, Meter_blob).

%%--------------------------------------------------------------------------
%% end
%%--------------------------------------------------------------------------


%% 警告消息
%% 推送消息
push_miss_msg(Meter_type, Meter) ->
    Msg_type = "alarm",
    AlarmType = "missing_data",
    Status = "on",
    Time = ?HELP:datetime_now_str(),
    Msg = "上报数据缺失",
    Msg_body = get_miss_msg_body([AlarmType, Status, Meter_type, Meter, Time, Msg]),
    analyze_push_service:push(Msg_type, Msg_body).

push_remove_miss_msg(Meter_type, Meter) ->
    Msg_type = "alarm",
    AlarmType = "missing_data",
    Status = "off",
    Time = ?HELP:datetime_now_str(),
    Msg = "解除上报缺失警告",
    Msg_body = get_miss_msg_body([AlarmType, Status, Meter_type, Meter, Time, Msg]),
    analyze_push_service:push(Msg_type, Msg_body).
    
get_miss_msg_body(Msg_body_list) ->
    string:join(Msg_body_list, "#").

%%--------------------------------------------------------------------------
%% 通讯质量
%%--------------------------------------------------------------------------

%% 计算通讯状况
cal_cq_weight(Cq_list) ->
    analyze_cq:cal_cq_weight(Cq_list).

%% 更新通讯质量
update_meter_cq(Meter_type, Meter, Cq_list) ->
    %% 计算通讯质量的权重
    Cq_weight = analyze_cq:cal_cq_weight(Cq_list),
    analyze_meter_to_cq:insert(Meter_type, Meter, Cq_weight).

%% 获取通讯质量
get_cq(Meter_type, Meter) ->
    analyze_meter_to_cq:lookup(Meter_type, Meter).

%%--------------------------------------------------------------------------
%% end
%%--------------------------------------------------------------------------


%%--------------------------------------------------------------------------
%% 上报的数据块
%%--------------------------------------------------------------------------

%% 更新最新的数据块
update_meter_blob(Meter_type, Meter, Meter_blob) ->
    analyze_meter_to_blob:insert({Meter_type, Meter}, Meter_blob).

%% 获取最新的数据块
%% get_meter_blob(Meter_type, Meter) -> {ok, Meter_blob} | {error, not_found}
get_meter_blob(Meter_type, Meter) ->
    analyze_meter_to_blob:lookup({Meter_type, Meter}).

%%--------------------------------------------------------------------------
%% end
%%--------------------------------------------------------------------------


%%--------------------------------------------------------------------------
%% 小时数据计算
%%--------------------------------------------------------------------------

%% 判断是否是小时整点
is_on_the_hour(Datetime, Avg_interval) ->
    {_Date, {_Hour, Mintue, _Second}} = Datetime,
    (Mintue =< Avg_interval).

%% 小时电能数据完整性
hour_data_start({Meter_type, Meter, Gateway, Cq_weight, Hour_data_last_datetime, Electric_power, Report_time, Is_on_the_hour}) ->
    analyze_meter_hour_data:start({Meter_type, Meter, Gateway, Cq_weight, Hour_data_last_datetime, Electric_power, Report_time, Is_on_the_hour}),
    case Is_on_the_hour of
        false -> ok;
        true  -> analyze_48hour_usedele:start(Report_time, Meter)
    end.

%% 两次上报期间是否是跨小时
is_across_the_hours(undefined, Report_time) ->
    false;
is_across_the_hours(Last_report_time, Report_time) when is_tuple(Last_report_time) ->
    {{Year_1, Month_1, Day_1}, {Hour_1, _, _}} = Last_report_time,
    {{Year_2, Month_2, Day_2}, {Hour_2, _, _}} = Report_time,
    Tmp_1 = {Year_1, Month_1, Day_1, Hour_1},
    Tmp_2 = {Year_2, Month_2, Day_2, Hour_2},
    (Tmp_1 =/= Tmp_2).

%% 小时平均温度
avg_temp_of_hour_start(Meter_type, Meter, Last_report_time) ->
    analyze_avg_temp_of_hour:start(Meter_type, Meter, Last_report_time).

%%--------------------------------------------------------------------------
%% end
%%--------------------------------------------------------------------------


%%--------------------------------------------------------------------------
%% 通宵活动
%%--------------------------------------------------------------------------

%% 是否监控该设备通宵用电
is_need_monitor_allnighter(Slave_label) ->
    lists:member(Slave_label, ?ALLNIGHTER_LABELS).

%% 设备是否通宵活动
is_allnighter(Datetime, Active_power) ->
    analyze_meter_allnighter:is_allnighter(Datetime, Active_power).

%%--------------------------------------------------------------------------
%% 校时操作
%%--------------------------------------------------------------------------

%% 是否需要校时
is_need_timing(Meter_time, Now) when (undefined =/= Meter_time) ->
    {DateTime_1, DateTime_2} = case (Meter_time < Now) of
        true  -> {Meter_time, Now};
        false -> {Now, Meter_time}       
    end,
    try ?HELP:getTimeDiff(DateTime_1, DateTime_2) >= ?TIMING_VALUE of
        true ->
           true;
        false ->
            false
    catch
        _:_ ->
            true
    end;
is_need_timing(Meter_time, Now) ->
    false.

%% 进行校时操作
timing_start(Meter_type, Meter, Cq_weight, Now) ->
    analyze_meter_timing:start(Meter_type, Meter, Cq_weight, Now).

%%--------------------------------------------------------------------------
%% end
%%--------------------------------------------------------------------------


%%--------------------------------------------------------------------------
%% 充值监控
%%--------------------------------------------------------------------------

%% 是否存在失败的充值记录 
%% is_exist_failure_of_recharge(Meter) -> false | {true, failure_record::tuple()}
is_exist_failure_of_recharge(Meter) ->
    analyze_recharge:is_exist_failure_of_recharge(Meter).
    
%% 对失败的充值记录做处理
recharge_start(Meter_type, Meter, Cq_weight, Unknown_record_of_recharge, Reverse_recharge_list)->
    analyze_recharge:start(Meter_type, Meter, Cq_weight, Unknown_record_of_recharge, Reverse_recharge_list).

%%--------------------------------------------------------------------------
%% end
%%--------------------------------------------------------------------------


%%--------------------------------------------------------------------------
%% 小时平均用电量
%%--------------------------------------------------------------------------

%% 每天的小时平均用电量
hour_avg_usedels_start(Meter_type, Meter, Report_time) ->
    analyze_hour_avg_usedele:start(Meter_type, Meter, Report_time).

%%--------------------------------------------------------------------------
%% end
%%--------------------------------------------------------------------------


%%--------------------------------------------------------------------------
%% 活动流
%%--------------------------------------------------------------------------

%% 活动流初始化
workflow_init(Meter_type, Meter, Slave_label) ->
    {ok, Workflow_module} = get_workflow_module_by_slave_label(Slave_label),
    Workflow_module:init(Meter_type, Meter).

%% 上报到来，更新活动流
update_workflow(Slave_label, Meter_type, Meter, Workflow, Meter_blob) ->
    {ok, Workflow_module} = get_workflow_module_by_slave_label(Slave_label),
    Workflow_module:update_workflow(Meter_type, Meter, Workflow, Meter_blob).

get_workflow_module_by_slave_label(Slave_label) ->
    case lists:keyfind(Slave_label, 1, ?SLAVE_LABEL_TO_MODULE)  of
        {Slave_label, Module_name} -> {ok, Module_name};
        false -> {error, Slave_label}
    end.

%%--------------------------------------------------------------------------
%% end
%%--------------------------------------------------------------------------

%%--------------------------------------------------------------------------
%% 时间日期相关的函数
%%--------------------------------------------------------------------------


is_a_new_month(DateTime_1, DateTime_2) when(DateTime_1 =:= undefined) orelse (DateTime_2 =:= undefined) ->
    false;
is_a_new_month(DateTime_1, DateTime_2) ->
    {{Year_1, Month_1, _}, _} = DateTime_1,
    {{Year_2, Month_2, _}, _} = DateTime_2,
    case (Year_1 =:= Year_2) andalso (Month_1 =:= Month_2) of
        true  -> false;
        false -> true           
    end.

%% 两个时间点的日期是否是不同
is_a_new_date(DateTime_1, DateTime_2) when(DateTime_1 =:= undefined) orelse (DateTime_2 =:= undefined) ->
    false;
is_a_new_date(DateTime_1, DateTime_2) ->
    {Date1, _} = DateTime_1,
    {Date2, _} = DateTime_2,
    case Date1 of
        Date2 -> false;
        _     -> true           
    end.


%%--------------------------------------------------------------------------
%% end
%%--------------------------------------------------------------------------


%%--------------------------------------------------------------------------
%% 中央空调低中高速使用时长
%%--------------------------------------------------------------------------

central_ac_used_time_start(Meter_type, Meter, Low_speed_used_time_str, Medium_speed_used_time_str, High_speed_used_time_str, Report_time) ->
    analyze_central_ac_used_time:start(Meter_type, Meter, Low_speed_used_time_str, Medium_speed_used_time_str, High_speed_used_time_str, Report_time).


%%--------------------------------------------------------------------------
%% end
%%--------------------------------------------------------------------------

%%--------------------------------------------------------------------------
%% 定额管理
%%--------------------------------------------------------------------------

%% 是否需要抄表来读取电量值
is_need_read_base_quantity(Last_report_time, Now) when is_tuple(Last_report_time) 
    andalso (Last_report_time =< Now) ->
    case ?HELP:getTimeDiff(Last_report_time, Now) >= 15*60*60 of
        true -> true;
        false -> false
    end;
is_need_read_base_quantity(_, _) ->
    true.

get_base_quantity(Meter_type, Meter, Last_report_time, Now, Cq_weight) when (Meter_type =:= ?AC_TYPE) orelse (Meter_type =:= ?SOCKET_TYPE) orelse (Meter_type =:= ?FOUR_WAY_SWITCH_TYPE) ->
    case analyze_meter_util:is_need_read_base_quantity(Last_report_time, Now) of
        false ->
            case analyze_meter_util:get_meter_blob(Meter_type, Meter) of
                {ok, Meter_blob} ->
                    Electric_power = analyze_meter_blob_util:get_electric_power_float(Meter_blob),
                    {ok, Electric_power};
                {error, _} ->
                    get_base_quantity_by_read(Meter_type, Meter, Cq_weight)
            end;
        true ->
            get_base_quantity_by_read(Meter_type, Meter, Cq_weight)
    end;
get_base_quantity(Meter_type, Meter, Last_report_time, Now, Cq_weight) when (Meter_type =:= ?CENTRAL_AC_TYPE) ->
    case analyze_meter_util:is_need_read_base_quantity(Last_report_time, Now) of
        false ->
            case analyze_meter_util:get_meter_blob(Meter_type, Meter) of
                {ok, Meter_blob} ->
                    Used_time_sum = analyze_meter_blob_util:get_low_medium_high_speed_used_time_sum(Meter_blob),
                    {ok, Used_time_sum / 60};
                {error, _} ->
                    get_base_quantity_by_read(Meter_type, Meter, Cq_weight)
            end;
        true ->
            get_base_quantity_by_read(Meter_type, Meter, Cq_weight)
    end.

get_base_quantity_by_read(Meter_type, Meter, Cq_weight) 
  when (Meter_type =:= ?AC_TYPE) orelse (Meter_type =:= ?SOCKET_TYPE) 
       orelse (Meter_type =:= ?FOUR_WAY_SWITCH_TYPE) ->
    Cmd_obj = #cmd_obj{
        eqpt_type = Meter_type, 
        eqpt_id_code = Meter, 
        cmd_type = "dsj", 
        cmd_id = "zhygzdn"
    },
    case send_cmd_to_middleware:send(Cmd_obj, Cq_weight) of
        {ok, Return_data} ->
            case send_cmd_to_middleware:get_result(Return_data) of
                {ok, Result_str} -> {ok, list_to_float(Result_str)};
                false -> {error, read_ele_is_fail}   
            end;
        _ ->
            {error, read_ele_is_fail}
    end;
get_base_quantity_by_read(Meter_type, Meter, Cq_weight) when (Meter_type =:= ?CENTRAL_AC_TYPE) ->
    Cmd_obj = #cmd_obj{
        eqpt_type = Meter_type, 
        eqpt_id_code = Meter, 
        cmd_type = "dsj", 
        cmd_id = "dnsjk"
    },
    case send_cmd_to_middleware:send(Cmd_obj, Cq_weight) of
        {ok, Return_data} ->
            case send_cmd_to_middleware:get_result(Return_data) of
                {ok, Result_str} -> 
                    case string:tokens(Result_str, ",") of
                        [_, _, _, _, _, _, Low_speed_used_time, Medium_speed_used_time, High_speed_used_time, _, _, _] ->
                            {ok, lists:sum([list_to_integer(X) || X <- [Low_speed_used_time, Medium_speed_used_time, High_speed_used_time]]) / 60};
                        _ ->
                            {error, read_ele_is_fail} 
                    end;
                false -> {error, read_ele_is_fail}   
            end;
        _ ->
            {error, read_ele_is_fail} 
    end.

meter_quota_start(Meter_type, Meter, Gateway, Cur_quantity) ->
    analyze_meter_quota:start(Meter_type, Meter, Gateway, Cur_quantity).

new_month_update_quota(Meter_type, Meter, Gateway, Cur_quantity) ->
     analyze_meter_quota:new_month_update_quota(Meter_type, Meter, Gateway, Cur_quantity).

%%--------------------------------------------------------------------------
%% end
%%--------------------------------------------------------------------------

%%--------------------------------------------------------------------------
%% 对表操作下发到网关相关操作
%%--------------------------------------------------------------------------

send_to_gateway_exec(Gateway, Meter_task = {Meter, Meter_type, Operation_type, Operation_argv}) ->
    case analyze_gateway_pid:lookup(Gateway) of
        {ok, Pid} ->
            case erlang:is_process_alive(Pid) of
                true ->
                    Pid ! {meter_task, self(), Meter_task};
                false ->
                    ?ERROR("gateway:~p process:~p is not alive~n", [Gateway, Pid])
            end,
            ok;
        {error, _} ->
            ?ERROR("not found pid of gateway:~p~n", [Gateway]),
            ok
    end.

%%--------------------------------------------------------------------------
%% end
%%--------------------------------------------------------------------------


%%--------------------------------------------------------------------------
%% 使用时长算法
%%--------------------------------------------------------------------------

get_used_time(Active_power_prev, Active_power_cur, Second_diff, Electric_power_diff) ->
    case {Active_power_prev >= ?WORKING_ACTIVE_POWER, Active_power_cur >= ?WORKING_ACTIVE_POWER} of
        {true, true} ->
            Second_diff;
        {true, false} ->
            %% 0.5h = 1800s
            case Second_diff =< 1800 of
                true -> 
                    Used_time_tmp = round(Electric_power_diff / (Active_power_prev / 1000) * ?ONE_HOUR_SECONDS),
                    select_min(Second_diff, Used_time_tmp);
                false -> 0  %% 忽略两点之间的时长
            end;
        {false, true} ->
            case Second_diff =< 1800 of
                true -> Second_diff;
                false -> 
                    Used_time_tmp = round(Electric_power_diff / (Active_power_cur / 1000) * ?ONE_HOUR_SECONDS),
                    select_min(Second_diff, Used_time_tmp)
            end;
        {false, false} ->
            0
    end.

select_min(Second_diff, Used_time_tmp) ->
    case Second_diff < Used_time_tmp of
        true ->
            Second_diff;
        false ->
            Used_time_tmp
    end.

%%--------------------------------------------------------------------------
%% end
%%--------------------------------------------------------------------------









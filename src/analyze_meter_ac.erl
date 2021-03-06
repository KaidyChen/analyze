-module(analyze_meter_ac).

-behaviour(gen_server).

-include("analyze.hrl").
-include("analyze_config.hrl").
-include("analyze_meter.hrl").
-include("cmd_obj.hrl").
-include("meter_quota.hrl").
-include("print.hrl").
-include("report.hrl").

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
]).

-record(state, {
    meter_field,                % 表的字段属性
    last_report_time,           % 最近一次上报的时间 
    miss_count = 0,             % 丢失数据的次数
    frist_miss_time,            % 首次丢失数据的时间
    cq_list,                    % 通讯质量列表
    report_rate_record,         % 数据上报率record
    used_data_last_year_month,  % 最后使用数据的年月
    all_nighter_push_strategy_list,  % 通宵用电推送机制列表
    hour_data_last_datetime,    % 小时数据的最后一条时间
    ac_work_activities          % 分体空调的活动
}).

-define(SOPTS, [
    {min_heap_size, 5000},
    {min_bin_vheap_size, 100000},
    {fullsweep_after, 500}
]).

%%===================================================================================
%% API
%%===================================================================================

%% @doc start a virtual device process
-spec(start_link(meter_field) -> {ok, pid()} | ignore | {error, Error :: {already_started, pid()} | term()}).
start_link(Meter_field) ->
    %{ok, _Pid} = gen_server:start_link(?MODULE, [Meter_field], [{spawn_opt, ?SOPTS}]).
    {ok, _Pid} = gen_server:start_link(?MODULE, [Meter_field], []).

%%===================================================================================
%% Gen_server callbacks
%%===================================================================================

init([Meter_field]) ->
    {Meter_type, Meter} = analyze_meter_field:get_meter_type_and_meter_by_meter_field(Meter_field),

    erlang:process_flag(trap_exit, true),
    analyze_meter_pid:insert({Meter_type, Meter}, self()),

    ReportInterval = analyze_util:get_report_interval(Meter_type),

    Second_timeout = ?MINUTE_TO_SECOND(ReportInterval),

    Now = ?HELP:datetime_now(),

    %% 活动
    Ac_work_activities = analyze_meter_ac_work_activities:work_activities_init(Meter_type, Meter, Now),

    %% 小时数据时间
    Hour_data_last_datetime = analyze_meter_hour_data:get_last_hour_data_datetime(Now, Meter),

    %% 月度使用数据年月
    Used_data_last_year_month = analyze_ac_used_data_of_month:get_used_data_last_year_month(Now, Meter_type, Meter),

    %% 初始化通讯状况
    Cq_list = [true, true, true, true, true, true],
    analyze_meter_util:update_meter_cq(Meter_type, Meter, Cq_list),
    
    Report_rate_record = #report_rate_record{
        %% 上报间隔
        second_timeout = Second_timeout,
        report_interval_timer = erlang:start_timer(?GET_TIMEOUT(Second_timeout), self(), ?REPORT_TIMEOUT_MSG),
        integrity_rate_start_time = ?HELP:datetime_now(),
        received = 0,
        should_be_received = 0
    },

    State = #state{
        meter_field = Meter_field,
        miss_count = 0, 
        cq_list = Cq_list,
        all_nighter_push_strategy_list = ?ALL_NIGHTER_INIT_PUSH_STRATEGY_LIST,
        report_rate_record = Report_rate_record,
        hour_data_last_datetime = Hour_data_last_datetime,
        used_data_last_year_month = Used_data_last_year_month,
        ac_work_activities = Ac_work_activities
    },
    {ok, State}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% 电能数据块上报
handle_info({report_come, Msg_type = ?DATAMSG, Meter_type, Meter, Meter_blob}, State) ->
    ?PRINT("~p/~p/~p/~p~n", [Msg_type, Meter_type, Meter, Meter_blob]),

    %% 上报时间
    Report_time = analyze_meter_blob_util:get_report_time(Meter_blob),

    #state{
       meter_field = Meter_field,
       last_report_time = Last_report_time,
       miss_count = Miss_count,
       cq_list = Cq_list,
       all_nighter_push_strategy_list = All_nighter_push_strategy_list,
       report_rate_record = Report_rate_record,
       ac_work_activities = Ac_work_activities
    } = State,

    %% 上报数据的完整率
    #report_rate_record{
        second_timeout = Second_timeout,
        report_interval_timer = Report_interval_timer,
        integrity_rate_start_time = Integrity_rate_start_time,
        received = Received,
        should_be_received = Should_be_received
    } = Report_rate_record,
    erlang:cancel_timer(Report_interval_timer),

    Should_be_received_tmp = Should_be_received + 1,
    Received_tmp = Received + 1,
    {New_Integrity_rate_start_time, New_received, New_should_be_received} = if
        Should_be_received_tmp >= (?ONE_HOUR_SECONDS div Second_timeout) ->
            analyze_data_integrity_rate:start({Meter, Integrity_rate_start_time, Report_time, Received_tmp, Should_be_received_tmp}),
            {Report_time, 0, 0};
        true ->
            {Integrity_rate_start_time, Received_tmp, Should_be_received_tmp}
    end,

    New_report_rate_record = Report_rate_record#report_rate_record{
        integrity_rate_start_time = New_Integrity_rate_start_time,
        report_interval_timer = erlang:start_timer(?GET_TIMEOUT(Second_timeout), self(), ?REPORT_TIMEOUT_MSG),
        received = New_received,
        should_be_received = New_should_be_received
    },

    %% 更新表的当前数据块信息
    analyze_meter_util:update_meter_blob(Meter_type, Meter, Meter_blob),

    %% 更新通讯状况代码
    New_cq_list = [true | lists:droplast(Cq_list)],
    analyze_meter_util:update_meter_cq(Meter_type, Meter, New_cq_list),

    %% 更新计算工作活动
    New_ac_work_activities = analyze_meter_ac_work_activities:update_work_activities(Ac_work_activities, Meter_blob, Meter_type, Meter),
    analyze_meter_work_activities_store:insert(Meter_type, Meter, New_ac_work_activities),

    %% 从标签

    Slave_label = analyze_meter_field:get_slave_label_by_meter_field(Meter_field),

    %% 功率 
    Active_power = analyze_meter_blob_util:get_active_power_float(Meter_blob),
    %% 是否监控该设备通宵用电
    New_all_nighter_push_strategy_list = case analyze_meter_util:is_need_monitor_allnighter(Slave_label) of
        false -> All_nighter_push_strategy_list;
        true -> 
            case analyze_meter_util:is_a_new_date(Last_report_time, Report_time) of
                true -> ?ALL_NIGHTER_INIT_PUSH_STRATEGY_LIST;
                false ->
                    case analyze_meter_util:is_allnighter(Report_time, Active_power) of
                        true -> 
                            analyze_meter_allnighter:update_all_nighter_push_strategy_list(All_nighter_push_strategy_list, Report_time, Meter_type, Meter);
                        false -> 
                            All_nighter_push_strategy_list  
                    end
            end
    end,

    case Miss_count >= ?MIN_MISS_COUNT of
        true ->
            analyze_meter_util:push_remove_miss_msg(Meter_type, Meter),
            ok;
        false ->
            ok
    end,   

    report_come_work(State, Msg_type, Meter_blob),

    NewState = State#state{
        last_report_time = Report_time,
        miss_count = 0, 
        cq_list = New_cq_list,
        all_nighter_push_strategy_list = New_all_nighter_push_strategy_list,
        report_rate_record = New_report_rate_record,
        ac_work_activities = New_ac_work_activities
    },
    %% 手动gc
    %% erlang:garbage_collect(self()),
    {noreply, NewState};

handle_info({report_come, Msg_type = ?WARNMSG, Meter_type, Meter, Data_field_str}, State) ->
    ?PRINT("~s/~s/~s/~s~n", [Msg_type, Meter_type, Meter, Data_field_str]), 
    {noreply, State};

%% 超时丢失数据
handle_info({timeout, TimerRef, ?REPORT_TIMEOUT_MSG}, State)  ->
    erlang:cancel_timer(TimerRef),
    #state{
        meter_field = Meter_field,
        miss_count = Miss_count, 
        cq_list = Cq_list,
        report_rate_record = Report_rate_record
    } = State,

    {Meter_type, Meter} = analyze_meter_field:get_meter_type_and_meter_by_meter_field(Meter_field),    

    #report_rate_record{
        second_timeout = Second_timeout,
        report_interval_timer = Report_interval_timer,
        integrity_rate_start_time = Integrity_rate_start_time,
        received = Received,
        should_be_received = Should_be_received
    } = Report_rate_record,
    erlang:cancel_timer(Report_interval_timer),

    Now = ?HELP:datetime_now(),

    %% 上报数据的完整率
    Should_be_received_tmp = Should_be_received + 1,
    {New_Integrity_rate_start_time, New_received, New_should_be_received} = if
        Should_be_received_tmp >= (?ONE_HOUR_SECONDS div Second_timeout) ->
            analyze_data_integrity_rate:start({Meter, Integrity_rate_start_time, Now, Received, Should_be_received_tmp}),
            {Now, 0, 0};
        true ->
            {Integrity_rate_start_time, Received, Should_be_received_tmp}
    end,

    New_report_rate_record = Report_rate_record#report_rate_record{
        integrity_rate_start_time = New_Integrity_rate_start_time,
        report_interval_timer = erlang:start_timer(?GET_TIMEOUT(Second_timeout), self(), ?REPORT_TIMEOUT_MSG),
        received = New_received,
        should_be_received = New_should_be_received
    },
    
    %% 更新通讯状况代码
    New_cq_list = [false | lists:droplast(Cq_list)],
    analyze_meter_util:update_meter_cq(Meter_type, Meter, New_cq_list),

    %% 触发数据严重缺失事件,
    case lists:member(Miss_count, ?ALRAM_MISS_NUMBER_LIST) of
        true ->
            analyze_meter_util:push_miss_msg(Meter_type, Meter),
            ?ERROR("~p/~p Alram: miss data ~p~n", [Meter_type, Meter, Miss_count]);
        false -> ok
    end,

    New_miss_count =
        case Miss_count < ?MAX_ALLOW_MISS_COUNT of
            true ->
                Miss_count + 1;
            false ->
                ?MAX_ALLOW_MISS_COUNT
        end,

    NewState = State#state{
        cq_list = New_cq_list,
        miss_count = New_miss_count,
        report_rate_record = New_report_rate_record
    },
    {noreply, NewState};

%% 设置定额
handle_info({set_quota, Quota, Mode}, State) ->
    #state {
        cq_list = Cq_list,
        last_report_time = Last_report_time,
        meter_field = Meter_field
    } = State,

    {Meter_type, Meter} = analyze_meter_field:get_meter_type_and_meter_by_meter_field(Meter_field),    

    Now = ?HELP:datetime_now(),

    Cq_weight = analyze_meter_util:cal_cq_weight(Cq_list),
    case analyze_meter_quota_server:get_quota(Meter_type, Meter) of
        {ok, Meter_quota} ->
            New_meter_quota = Meter_quota#meter_quota{  
                quota = Quota,          
                mode = Mode,
                warn_time_1 = 0,  
                warn_time_2 = 0    

            },
            %% 保存定额信息
            analyze_meter_quota_server:update_quota(New_meter_quota);
        {error, _} ->
            case analyze_meter_util:get_base_quantity(Meter_type, Meter, Last_report_time, Now, Cq_weight) of
                {ok, Electric_power} ->
                    New_meter_quota = #meter_quota{
                        key = {Meter_type, Meter},
                        base_quantity = Electric_power,   
                        quota = Quota,          
                        mode = Mode, 
                        cur_quantity = Electric_power,          
                        warn_time_1 = 0,  
                        warn_time_2 = 0    
                    },
                    %% 保存定额信息
                    analyze_meter_quota_server:update_quota(New_meter_quota);
                {error, Reason} ->
                    ?ERROR("~p/~p can't get the last electric_power:~p~n", [Meter_type, Meter, Reason])
            end
    end,
    {noreply, State};

handle_info({add_recharge_record, Order_no, Recharge_num, Recharge_date, Recharge_time, Recharge_money, Recharge_status}, State) ->
    #state {
        meter_field = Meter_field
    } = State,

    {Meter_type, Meter} = analyze_meter_field:get_meter_type_and_meter_by_meter_field(Meter_field),    

    case analyze_meter_recharge:is_exist_unknown_of_recharge(Meter_type, Meter) of
        false ->
            analyze_meter_recharge:add_recharge_record(Meter_type, Meter, {Order_no, Recharge_num, Recharge_date, Recharge_time, Recharge_money, Recharge_status});
        true ->
            ?ERROR("is_exist_unknown_of_recharge is return true.~n", [])
    end,
    {noreply, State};

handle_info({meter_task_result, Meter_task, Result}, State) ->
    ?ERROR("meter_task:~p result:~p~n", [Meter_task, Result]),
    {noreply, State};

%% 整点小时数据
handle_info({hour_data_on_the_hour, Datetime, Electric_power}, State) ->
    #state {
        meter_field = Meter_field,
        hour_data_last_datetime = Hour_data_last_datetime
    } = State,

    ?ERROR("hour_data_on_the_hour:~p ~p~n", [Datetime, Electric_power]),
    {Meter_type, Meter} = analyze_meter_field:get_meter_type_and_meter_by_meter_field(Meter_field),    

    New_hour_data_last_datetime = case Datetime > Hour_data_last_datetime of
        true ->
            ?ERROR("Datetime > Hour_data_last_datetime:~p ~p~n", [Datetime, Electric_power]),
            analyze_meter_hour_data:save_hour_data(Datetime, Meter, Electric_power),
            Datetime;
        false ->
            Hour_data_last_datetime
    end,

    NewState = State#state{
        hour_data_last_datetime = New_hour_data_last_datetime
    },
    {noreply, NewState};

%% 小时数据结果
handle_info({hour_data_result, false}, State) ->
    {noreply, State};
handle_info({hour_data_result, {ok, Result_str_tmp}}, State) ->
    #state {
        cq_list = Cq_list,
        last_report_time = Last_report_time,
        meter_field = Meter_field,
        hour_data_last_datetime = Hour_data_last_datetime
    } = State,

    {Meter_type, Meter} = analyze_meter_field:get_meter_type_and_meter_by_meter_field(Meter_field),    

    New_hour_data_last_datetime = case analyze_meter_hour_data:parse_result(Result_str_tmp) of
        {ok, Datetime, Electric_power} ->
            case Datetime > Hour_data_last_datetime of
                true ->
                    ?ERROR("Hour_data_last_datetime~p~n", [Hour_data_last_datetime]),
                    analyze_meter_hour_data:save_hour_data(Datetime, Meter, Electric_power),
                    Datetime;
                false ->
                    Hour_data_last_datetime
            end;
        {error, _} ->
            Hour_data_last_datetime
    end,

    NewState = State#state{
        hour_data_last_datetime = New_hour_data_last_datetime
    },
    {noreply, NewState};

%% 月度使用数据结果
handle_info({used_data_of_month_result, false}, State) ->
    {noreply, State};
handle_info({used_data_of_month_result, {ok, Result_str_tmp}}, State) ->
    #state {
        cq_list = Cq_list,
        last_report_time = Last_report_time,
        meter_field = Meter_field,
        used_data_last_year_month = Used_data_last_year_month
    } = State,

    {Meter_type, Meter} = analyze_meter_field:get_meter_type_and_meter_by_meter_field(Meter_field),    

    ?ERROR("used_data_of_month_result:~p~n", [Result_str_tmp]),

    NewState = case analyze_ac_used_data_of_month:parse_result(Result_str_tmp) of
        {ok, {Year_data, Month_data} = Data_year_month, Data_item_list} ->
            case analyze_ac_used_data_of_month:is_can_write(Used_data_last_year_month, Data_year_month) of
                true ->
                    analyze_ac_used_data_of_month:save_hour_data(Data_year_month, Meter, Data_item_list),
                    State#state{used_data_last_year_month = Data_year_month};
                false ->
                    State
            end;
        {error, Reason} ->
            ?ERROR("used_data_of_month_result:~p is error:~p~n", [Result_str_tmp, Reason]),
            State
    end,
    {noreply, NewState};


%% 结束虚拟表
handle_info(del_meter, State) ->
    Reason = shutdown,
    {stop, Reason, State};

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, State = #state{meter_field = Meter_field}) ->
    {Meter_type, Meter} = analyze_meter_field:get_meter_type_and_meter_by_meter_field(Meter_field),
    analyze_meter_pid:delete({Meter_type, Meter}),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%=============================================================================
%% Internal functions
%%=============================================================================

report_come_work(State, Msg_type, Meter_blob) ->
    #state{
        meter_field = Meter_field,
        last_report_time = Last_report_time,
        cq_list = Cq_list,
        hour_data_last_datetime = Hour_data_last_datetime,
        used_data_last_year_month = Used_data_last_year_month
    } = State,

    {Meter_type, Meter} = analyze_meter_field:get_meter_type_and_meter_by_meter_field(Meter_field),
    Gateway = analyze_meter_field:get_gateway_by_meter_field(Meter_field),
    Build_id = analyze_meter_field:get_build_id_by_meter_field(Meter_field),
    Master_label = analyze_meter_field:get_master_label_by_meter_field(Meter_field),
    Slave_label = analyze_meter_field:get_slave_label_by_meter_field(Meter_field),

    Cq_weight = analyze_meter_util:cal_cq_weight(Cq_list),

    %% 上报数据触发执行任务
    analyze_meter_util:run_tasks(Meter_type, Meter, Msg_type, Meter_blob),

    %% 本次上报的时间
    Report_time = analyze_meter_blob_util:get_report_time(Meter_blob),

    %% 功率 
    Active_power = analyze_meter_blob_util:get_active_power_float(Meter_blob),

    %% 电量
    Electric_power = analyze_meter_blob_util:get_electric_power_float(Meter_blob),

    %% 定额管理
    case analyze_meter_util:is_a_new_month(Last_report_time, Report_time) of
        false -> analyze_meter_util:meter_quota_start(Meter_type, Meter, Gateway, Electric_power);
        true -> analyze_meter_util:new_month_update_quota(Meter_type, Meter, Gateway, Electric_power) 
    end,
    
    % 每日平均小时用电量
    analyze_meter_util:hour_avg_usedels_start(Meter_type, Meter, Report_time),

    %% 小时平均室温
    case analyze_meter_util:is_across_the_hours(Last_report_time, Report_time) of
        false -> ok;
        true  -> analyze_meter_util:avg_temp_of_hour_start(Meter_type, Meter, Last_report_time)
    end,

    Meter_time = analyze_meter_blob_util:get_datetime(Meter_blob),
    %% 校时
    case analyze_meter_util:is_need_timing(Meter_time, Report_time) of
        false -> ok;
        true  -> analyze_meter_util:timing_start(Meter_type, Meter, Cq_weight, Report_time)
    end,

    %% 月度使用数据
    analyze_ac_used_data_of_month:start({Meter_type, Meter, Gateway, Cq_weight, Used_data_last_year_month, Report_time}),

    %% 整点小时电量数据完备性
    Avg_interval = ?AVG_INTERVAL_MINUTE, % 平均时间间隔
    Is_on_the_hour = analyze_meter_util:is_on_the_hour(Report_time, Avg_interval),
    analyze_meter_util:hour_data_start({Meter_type, Meter, Gateway, Cq_weight, Hour_data_last_datetime, Electric_power, Report_time, Is_on_the_hour}),

    % 充值操作
    case analyze_meter_recharge:get_last_record_of_recharge(Meter_type, Meter) of
        {ok, Last_record_of_recharge} ->
            case analyze_meter_recharge:is_exist_unknown_of_recharge(Meter_type, Meter, Last_record_of_recharge) of
                    false ->
                        ok;
                    true ->
                        try analyze_meter_recharge:start(Meter_type, Meter, Cq_weight, Last_record_of_recharge) of
                            _ ->
                                ok
                        catch
                            Class:What ->
                                ?ERROR("catch ~p:~p~n", [Class, What])
                        end    
            end;
        {error, Reason} ->
            ok
    end,

    ok.

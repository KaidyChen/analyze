-module(analyze_meter_norter_cold_water).

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
          miss_count = 0, 
          frist_miss_time, 
          cq_list, 
          report_rate_record,         % 数据上报率record
          hour_data_last_datetime    % 小时数据的最后一条时间
         }).

%%===================================================================================
%% API
%%===================================================================================

%% @doc start a virtual device process
-spec(start_link(meter_field) -> {ok, pid()} | ignore | {error, Error :: {already_started, pid()} | term()}).
start_link(Meter_field) ->
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

    %% 初始化通讯状况
    Cq_list = [true, true, true, true, true, true],
    analyze_meter_util:update_meter_cq(Meter_type, Meter, Cq_list),

    Hour_data_last_datetime = analyze_meter_hour_data:get_last_hour_data_datetime(Now, Meter),

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
               report_rate_record = Report_rate_record,
               hour_data_last_datetime = Hour_data_last_datetime
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
       cq_list = Cq_list,
       miss_count = Miss_count,
       report_rate_record = Report_rate_record
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
    {New_Integrity_rate_start_time, New_received, New_should_be_received} = 
        if
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
    %analyze_meter_util:update_meter_blob(Meter_type, Meter, Meter_blob),

    %% 更新通讯状况代码
    New_cq_list = [true | lists:droplast(Cq_list)],
    analyze_meter_util:update_meter_cq(Meter_type, Meter, New_cq_list),

    %report_come_work(State, Msg_type, Meter_blob),

    NewState = State#state{
                 last_report_time = analyze_meter_blob_util:get_report_time(Meter_blob),
                 miss_count = 0, 
                 cq_list = New_cq_list,
                 report_rate_record = New_report_rate_record
                },
    %% 手动gc

    case Miss_count >= ?MIN_MISS_COUNT of
        true ->
            analyze_meter_util:push_remove_miss_msg(Meter_type, Meter),
            ok;
        false ->
            ok
    end,

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
    {New_Integrity_rate_start_time, New_received, New_should_be_received} = 
        if
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

    New_hour_data_last_datetime = 
        case analyze_meter_hour_data:parse_result(Result_str_tmp) of
            {ok, Datetime, Electric_power} ->
                case Datetime > Hour_data_last_datetime of
                    true ->
                        %% push data of hour
                        analyze_push_data_server:push_hour_data(Meter, Datetime, Electric_power),
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

%% 上报数据正好是整点，间隔一个小时
handle_info({hour_data_on_the_hour, Datetime, Electric_power}, State) ->
    #state {
       meter_field = Meter_field,
       hour_data_last_datetime = Hour_data_last_datetime
      } = State,

    {Meter_type, Meter} = analyze_meter_field:get_meter_type_and_meter_by_meter_field(Meter_field),
    
    New_hour_data_last_datetime = 
        case Datetime > Hour_data_last_datetime of
            true ->
                analyze_push_data_server:push_hour_data(Meter, Datetime, Electric_power), 
                analyze_meter_hour_data:save_hour_data(Datetime, Meter, Electric_power),
                Datetime;
            false ->
                Hour_data_last_datetime
        end,

    NewState = State#state{
                 hour_data_last_datetime = New_hour_data_last_datetime
                },
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
       hour_data_last_datetime = Hour_data_last_datetime
      } = State,

    {Meter_type, Meter} = analyze_meter_field:get_meter_type_and_meter_by_meter_field(Meter_field),
    Gateway = analyze_meter_field:get_gateway_by_meter_field(Meter_field),
    Build_id = analyze_meter_field:get_build_id_by_meter_field(Meter_field),
    Master_label = analyze_meter_field:get_master_label_by_meter_field(Meter_field),
    Slave_label = analyze_meter_field:get_slave_label_by_meter_field(Meter_field),

    Cq_weight = analyze_meter_util:cal_cq_weight(Cq_list),

    %% 上报数据触发执行任务
    analyze_meter_util:run_tasks(Meter_type, Meter, Msg_type, Meter_blob),

    Report_time = analyze_meter_blob_util:get_report_time(Meter_blob),

    Meter_time = analyze_meter_blob_util:get_datetime(Meter_blob),

    %% 电量
    Electric_power = analyze_meter_blob_util:get_electric_power_float(Meter_blob),

    %% 定额管理
    case analyze_meter_util:is_a_new_month(Last_report_time, Report_time) of
        false -> analyze_meter_util:meter_quota_start(Meter_type, Meter, Gateway, Electric_power);
        true -> analyze_meter_util:new_month_update_quota(Meter_type, Meter, Gateway, Electric_power) 
    end,

    %% 校时
    case analyze_meter_util:is_need_timing(Meter_time, Report_time) of
        false -> ok;
        true  -> analyze_meter_util:timing_start(Meter_type, Meter, Cq_weight, Report_time)
    end,

    %% 整点小时数据完备性
    case Meter_time of
        undefined -> ok;
        Meter_time when is_tuple(Meter_time) -> 
            Avg_interval = ?AVG_INTERVAL_MINUTE, % 平均时间间隔
            Electric_power = analyze_meter_blob_util:get_electric_power_float(Meter_blob),
            Is_on_the_hour = analyze_meter_util:is_on_the_hour(Report_time, Avg_interval),
            analyze_meter_util:hour_data_start({Meter_type, Meter, Gateway, Cq_weight, Hour_data_last_datetime, Electric_power, Report_time, Is_on_the_hour})
    end,

                                                % % 充值操作
                                                % case analyze_meter_util:is_exist_failure_of_recharge(Meter) of
                                                %     false -> ok;
                                                %     {true, Unknown_record_of_recharge, Reverse_recharge_list} -> 
                                                %         analyze_meter_util:recharge_start(Meter_type, Meter, Cq_weight, Unknown_record_of_recharge, Reverse_recharge_list)    
                                                % end,

    ok.



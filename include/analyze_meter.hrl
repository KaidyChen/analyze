%% 分体空调上报的数据字段
-record(ac_info, {
    electric_power,         % 当前组合有功总电能
    voltage,                % 电压
    electric_current,       % 电流
    active_power,           % 功率
    temp,                   % 环境温度
    power_system_frequency, % 电网频率
    power_factor,           % 功率因子
    relay_status,           % 继电器状态
    status_word,            % 状态字
    datetime                % 设备的时标
}).

%% 插座上报的数据字段
-record(socket_info, {
    electric_power,         % 当前组合有功总电能
    voltage,                % 电压
    electric_current,       % 电流
    active_power,           % 功率
    power_system_frequency, % 电网频率
    power_factor,           % 功率因子
    relay_status,           % 继电器状态
    status_word,            % 状态字
    datetime                % 设备的时标
}).


%% 中央空调上报的数据字段
-record(central_ac_info, {
    electric_power,         % 当前组合有功总电能
    active_power,           % 功率
    temp,                   % 环境温度
    relay_status,           % 继电器状态
    temp_and_mode,          % 设定温度及模式
    wind_speed_gears,       % 风速档位
    low_speed_used_time,    % 低速累计使用时长
    medium_speed_used_time, % 中速累计使用时长
    high_speed_used_time,   % 高速累计使用时长
    amount,                 % 剩余金额
    status_word,            % 状态字
    datetime                % 设备的时标
}).

%% 三相电表
-record(three_phase_info, {
          electric_power,
          active_power,
          datetime
         }).

%% 空气检测仪
-record(air_detector_info, {
          pm1dot0,
          pm2dot5,
          pm10,
          temperature,
          humidity,
          hcho,
          tvoc,
          co2,
          co,
          o2,
          datetime
         }).

-record(blob, {
        report_time :: {}, 
        info :: #ac_info{} | #socket_info{} | #central_ac_info{} |
                #three_phase_info{} | #air_detector_info{}
}).

%% 卡座的活动流
-record(workflow, {
    prev_blob :: #blob{} | undefined,   % 前一个上报的数据块
    status,             % 当前状态
    platform_list,      % 功率平台列表, 一个平台也就是一个功率列表
    mean_max,           % 每个功率平台平均值的最大值  
    sum_work_status,    % 非关机时的总功率
    count,              % 非关机的总数据点数
    starting_up_time,   % 开机时间点
    power_off_time,     % 关机时间点
    sum_worktime        % 今日总工作时长
}).

%% 工作时的平均室温
-record(work_temp, {
    work_avg_temp,     % 工作状态的平均室温
    sum_work_temp,     % 工作状态总的平均室温 
    work_record_count  % 工作状态总的记录数 
}).

%% 分体空调活动记录
-record(ac_work_activities, {
    prev_blob :: #blob{} | undefined, 
    sum_worktime,   % 总的工作时长
    sum_used_ele,   % 总的用电量
    work_prev_temp, % 工作前的室温
    work_temp :: #work_temp{}
}).

%% 中央空调活动记录
-record(central_ac_work_activities, {
    prev_blob :: #blob{} | undefined, 
    sum_worktime,   % 总的工作时长
    work_prev_temp, % 工作前的室温
    work_temp :: #work_temp{}
}).

-record(report_rate_record, {
    second_timeout,             % 超时时间/秒
    report_interval_timer,      % 超时定时器
    integrity_rate_start_time,  % 数据完整率时间区间的开始时间
    received,                   % 收到的上报次数
    should_be_received          % 理应收到的上报次数
}).

%% 通宵开机推送次数机制
-define(ALL_NIGHTER_INIT_PUSH_STRATEGY_LIST, [{21, 0}, {22, 0}, {23, 0}]).

%% 上报超时消息
-define(REPORT_TIMEOUT_MSG, report_timeout_msg).

-define(MINUTE_TO_SECOND(Minute), (Minute * 60)).
-define(GET_TIMEOUT(Seconds), (Seconds * 1000)).
-define(AVG_INTERVAL_MINUTE, 12).

-define(EVENT_MISS_COUNT, 4).

-define(MAX_ALLOW_MISS_COUNT, 100).

-define(MIN_MISS_COUNT, 3).
-define(ALRAM_MISS_NUMBER_LIST, [3, 6, 9]).


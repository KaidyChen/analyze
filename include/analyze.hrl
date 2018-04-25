%% Application name
-define(APP, analyze).


-define(DEFAULT_REPORT_INTERVAL, 15).

%% Gen_tcp connect opt
-define(CONN_OPTS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}, {send_timeout, ?SOCKET_TIMEOUT}]).

%% EVENT事件的模块名
-define(EVENT, analyze_event).
%% 辅助函数的模块名
-define(HELP, help).
-define(HELPER, helper_util).
%% 计算指标的辅助函数模块名
-define(CALHELP, calHelp).

%% 推送消息类型
-define(INFO, "info").
-define(WARN, "warn").

%% 建筑类型
-define(ROOM, room).
-define(FLOOR, floor).
-define(BUILDING, building).
-define(GARDEN, garden).
%% 建筑id的长度
-define(ROOM_ID_LEN, 16).
-define(FLOOR_ID_LEN, 12).
-define(BUILDING_ID_LEN, 8).
-define(GARDEN_ID_LEN, 4).

%% 中间件命令操作类型
-define(TZ, "tz").
-define(HZ, "hz").

%% 设备类型定义
% 分体空调
-define(AC_TYPE, "0a0001aa7k").             
% 插座
-define(SOCKET_TYPE, "0a0001a820").        
% 四路开关面板
-define(FOUR_WAY_SWITCH_TYPE, "0a0001a830"). 
% 照明灯
-define(LIGHTING_TYPE, "0a0001a840").       
% 中央空调
-define(CENTRAL_AC_TYPE, "0a0001a4r5").
% 单相电表
-define(SINGLE_PHASE_TYPE, "0a0001a03c").
% 三相电表
-define(THREE_PHASE_TYPE, "0a0001bwhl").
% 国网三相多功能表
-define(GW_THREE_PHASE_TYPE, "0a0001bjxr").
% 北电冷水表
-define(NORTEL_COLD_WATER_TYPE, "0a0002a3aq").
% 空气检测仪
-define(AIR_DETECTOR_TYPE, "0a0001a880").
     
%% 用电计量表类型列表
-define(USED_ELE_METER_TYPE_LIST, 
        [
         ?AC_TYPE,
         ?SOCKET_TYPE,
         ?FOUR_WAY_SWITCH_TYPE,
         ?LIGHTING_TYPE,
         ?CENTRAL_AC_TYPE,
         ?SINGLE_PHASE_TYPE,
         ?THREE_PHASE_TYPE,
         ?GW_THREE_PHASE_TYPE
        ]).

%% 用电时长计量表类型列表
-define(USED_TIME_METER_TYPE_LIST, 
        [
         ?AC_TYPE,
         ?SOCKET_TYPE,
         ?FOUR_WAY_SWITCH_TYPE,
         ?LIGHTING_TYPE,
         ?CENTRAL_AC_TYPE
        ]).

%% 设备类型对应的模块 
-define(METER_TYPE_TO_MODULE, 
        [
         {?SINGLE_PHASE_TYPE, analyze_meter_single_phase},
         {?THREE_PHASE_TYPE, analyze_meter_three_phase},
         {?GW_THREE_PHASE_TYPE, analyze_meter_three_phase},
         {?NORTEL_COLD_WATER_TYPE, analyze_meter_norter_cold_water},
         {?AC_TYPE, analyze_meter_ac},
         {?SOCKET_TYPE, analyze_meter_socket},
         {?FOUR_WAY_SWITCH_TYPE, analyze_meter_lighting},
         {?LIGHTING_TYPE, analyze_meter_lighting},
         {?CENTRAL_AC_TYPE, analyze_meter_central_ac},
         {?AIR_DETECTOR_TYPE, analyze_meter_air_detector}
        ]).

%% 设备类型对应的文件夹名
-define(METER_TYPE_TO_DIRNAME, 
        [
         {?AC_TYPE, "ac"},
         {?SOCKET_TYPE, "socket"},
         {?FOUR_WAY_SWITCH_TYPE, "four_way_switch"},
         {?LIGHTING_TYPE, "lighting"},
         {?CENTRAL_AC_TYPE, "central_ac"}
        ]).

%%------------------------------------------------------------
%% 能源标签
%%------------------------------------------------------------

%% 若没有从标签，则有个默认值
-define(DEFAULT_SLAVE_LABEL, "null").

%% 空调标签
-define(AC, "ac").
%% 卡座标签
-define(SOCKET, "socket").
%% 照明标签
-define(LIGHTING, "lighting").


%%------------------------------------------------------------
%% end
%%------------------------------------------------------------


%%------------------------------------------------------------
%% 从标签
%%------------------------------------------------------------

%% 空调类从标签

%% 办公室空调标签
-define(OFFICE_AC, "office_ac").
%% 宿舍空调标签
-define(DORMITORY_AC, "dormitory_ac").


%% 插座类从标签

%% 电脑插座标签
-define(PC_SOCKET, "pc_socket").

%% 办公室照明标签
-define(OFFICE_LIGHTING, "office_lighting").
%% 宿舍照明标签
-define(DORMITORY_LIGHTING, "dormitory_lighting").


%% 从标签对应的活动流模块
-define(SLAVE_LABEL_TO_MODULE, 
        [
         {?PC_SOCKET, analyze_workflow_pc}
        ]).

%%------------------------------------------------------------
%% end
%%------------------------------------------------------------

%%------------------------------------------------------------
%% 建筑能源
%%------------------------------------------------------------
-define(COAL_COEFFICIENT_LIST, 
        [
         {?AC, 0.1229},
         {?SOCKET, 0.1229},
         {?LIGHTING, 0.1229}
        ]).

%% 要计算通宵开机的能源标签
-define(ALLNIGHTER_LABELS, [?OFFICE_AC]).

%%------------------------------------------------------------
%% end
%%------------------------------------------------------------

%% 通宵开机的起始时间
-define(ALLNIGHTER_START_TIME, {21, 0, 0}).
-define(ALLNIGHTER_END_TIME, {6, 0, 0}).
%% 开机的功率阈值
-define(STARTING_UP_ACTIVE_POWER, 7.0).


%% XML report data item
-define(TOTAL_ACTIVE_POWER, total_active_power).
-define(A_ACTIVE_POWER, a_active_power).
-define(B_ACTIVE_POWER, b_active_power).
-define(C_ACTIVE_POWER, c_active_power).
-define(TOTAL_REACTIVE_POWER, total_reactive_power).
-define(A_REACTIVE_POWER, a_reactive_power).
-define(B_REACTIVE_POWER, b_reactive_power).
-define(C_REACTIVE_POWER, c_reactive_power).
-define(TOTAL_POWER_FACTOR, total_power_factor).
-define(A_POWER_FACTOR, a_power_factor).
-define(B_POWER_FACTOR, b_power_factor).
-define(C_POWER_FACTOR, c_power_factor).
-define(A_VOLTAGE, a_voltage).
-define(B_VOLTAGE, b_voltage).
-define(C_VOLTAGE, c_voltage).
-define(A_CURRENT, a_current).
-define(B_CURRENT, b_current).
-define(C_CURRENT, c_current).
-define(ZERO_CURRENT, zero_current).
-define(TOTAL_APPARENT_POWER, total_apparent_power).
-define(A_APPARENT_POWER, a_apparent_power).
-define(B_APPARENT_POWER, b_apparent_power).
-define(C_APPARENT_POWER, c_apparent_power).
-define(POSITIVE_ACTIVE_ELE, positive_active_ele).
-define(RATE1_POSITIVE_ACTIVE_ELE, rate1_positive_active_ele).
-define(RATE2_POSITIVE_ACTIVE_ELE, rate2_positive_active_ele).
-define(RATE3_POSITIVE_ACTIVE_ELE, rate3_positive_active_ele).
-define(RATE4_POSITIVE_ACTIVE_ELE, rate4_positive_active_ele).
-define(INVERSE_ACTIVE_ELE, inverse_active_ele).
-define(RATE1_INVERSE_ACTIVE_ELE, rate1_inverse_active_ele).
-define(RATE2_INVERSE_ACTIVE_ELE, rate2_inverse_active_ele).
-define(RATE3_INVERSE_ACTIVE_ELE, rate3_inverse_active_ele).
-define(RATE4_INVERSE_ACTIVE_ELE, rate4_inverse_active_ele).
-define(TOTAL_WATER_CONSUMPTION, total_water_consumption).

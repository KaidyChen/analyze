%% 配置信息

%%%====================================================
%% 配置文件的宏定义
%%%====================================================

%% 节假日配置文件
-define(HOLIDAY_CONF, "rel/holiday.config").
%% 设备表的配置信息文件
-define(METERS_CONFIG, "rel/meters.config").
%% 上报触发式任务的配置文件
-define(TASKS_CONFIG, "rel/taskfile.config").
%% Crontab 定时任务
-define(CRONTAB_FILE, "rel/crontab.config").
%% 上报间隔配置文件
-define(REPORT_INTERVAL_CONFIG, "./rel/report_interval.config").

%%%====================================================
%% End 配置文件的宏定义
%%%====================================================

%%%====================================================
%% 数据名称的宏定义
%%%====================================================

%% 无数据项
-define(NONE_DATA, "--").
%% 无数据
-define(NULL, "null").

%% Socket API 超时时间
-define(SOCKET_TIMEOUT, 5*1000).

%% 换行符
-define(NL, "\n").
%% 字段分割符
-define(FS, " ").

%% 设备号长度
-define(DEVICEIDLEN, 12).

%% 工作时段
-define(WORKTIME_STERT, {9, 0, 0}).
-define(WORKTIME_END, {18, 0, 0}).

-define(ONE_HOUR_SECONDS, 3600).

%% 需要校时的秒数
-define(TIMING_VALUE, (5 * 60)).

%%%====================================================
%% End 数据名称的宏定义
%%%====================================================


%%%====================================================
%% 数据存储相关的的宏定义
%%%====================================================

%% 文件存储的后缀名
-define(SUFFIX, ".txt").

%% 楼宇数据存储的目录
-define(BUILDDIR, "./build").
%% 设备表数据存储的目录
-define(METERDIR, "./meter").
%% 排名保存的目录
-define(RANGKING_DIR, "./ranking").


%% 工作最低功率
-define(WORKING_ACTIVE_POWER, 7.0).

%% 理论舒适温度，用于计算空调可节省多少电量
-define(THEORY_TEMP, 26.0).
%% 制暖温度界限值
-define(HEATING_TEMP, 22.0).

%% 电量差大于它则判断为用电
-define(USEDELEDIFF, 0.012).
%% 节能标准：日平均小时用电量小于它就判定为节能
-define(SAVESTANDARD, 1.2).
-define(AVGHOURDIR, "avg_hour").

%% 表充值记录的目录名
-define(RECHARGEDIR, "recharge").

%% 工作流的目录名
-define(WORKFLOW, "work_flow").
%% 空调工作活动的目录名
-define(WORK_ACTIVITIES, "work_activities").

%% 表整点小时数据的目录名
-define(HOURDATADIR, "hour_data").
%% 空调小时平均温度的目录名
-define(AVG_TEMP_DIR, "avg_temp_of_hour").
%% 日平均小时用电目录名
-define(HOUR_AVG_USES_ELE, "hour_avg_uses_ele").

%% 空调日可节省电量
-define(CONSERVE_ELE_OF_DAY_DIR, "conserve_ele_of_day").
%% 空调月可节省电量
-define(CONSERVE_ELE_OF_MONTH_DIR, "conserve_ele_of_month").
%% 日用电量目录名
-define(USED_ELE_OF_DAY_DIR, "used_ele_of_day").
%% 月用电量目录名
-define(USED_ELE_OF_MONTH_DIR, "used_ele_of_month").
%% 分项用电量目录名
-define(SUBENTRY_USED_ELE_OF_MONTH_DIR, "subentry_used_ele_of_month").
%% 工作日的用电量(包括工作时段和非工作时段)目录名
-define(USED_ELE_OF_WORKDAY_DIR, "used_ele_of_workday").
%% 月工作日工作时段/非工作时段用电量的目录名
-define(USED_ELE_OF_WORKDAY_MONTH_DIR, "used_ele_of_workday_month").
%% 工作日的用电时长(包括工作时段和非工作时段)目录名
-define(USED_TIME_OF_WORKDAY_DIR, "used_time_of_workday").
%% 月工作日工作时段/非工作时段用电时长的目录名
-define(USED_TIME_OF_WORKDAY_MONTH_DIR, "used_time_of_workday_month").
%% 月工作日与非工作日的用电量目录名
-define(WORK_AND_NONWORK_USED_ELE_OF_MONTH_DIR, "work_and_nonwork_used_ele_of_month").
%% 月工作日与非工作日的用电时长目录名
-define(WORK_AND_NONWORK_USED_TIME_OF_MONTH_DIR, "work_and_nonwork_used_time_of_month").


%% 月度冻结数据目录
-define(FROZEN_DATA_OF_MONTH, "frozen_data_of_month").

%% 月度使用数据目录
-define(USED_DATA_OF_MONTH, "used_data_of_month").

%% 中央空调使用时长
-define(CENTRAL_AC_USED_TIME, "used_time_of_central_ac").

%% 日使用时长目录名
-define(USED_TIME_OF_DAY_DIR, "used_time_of_day").
%% 月使用时长目录名
-define(USED_TIME_OF_MONTH_DIR, "used_time_of_month").

%% 中央空调三档日使用时长
-define(THREE_SPEED_USED_TIME_OF_DAY_DIR, "three_speed_used_time_of_day").
%% 中央空调三档月使用时长
-define(THREE_SPEED_USED_TIME_OF_MONTH_DIR, "three_speed_used_time_of_month").

%% 每天通宵使用时长目录名
-define(USED_TIME_ALL_NIGHT_OF_DAY_DIR, "used_time_all_night_of_day").
%% 每月通宵使用时长目录名
-define(USED_TIME_ALL_NIGHT_OF_MONTH_DIR, "used_time_all_night_of_month").

%% 上报完整率
%% 表数据完整率的目录名(一个月一个文件，每条记录一个小时内的完整率)
-define(INTEGRITY_RATE_DIR, "integrity_rate").
%% 每日平均数据完整率的目录名
-define(INTEGRITY_RATE_OF_DAY_DIR, "integrity_rate_of_day").
%% 每月平均数据完整率的目录名
-define(INTEGRITY_RATE_OF_MONTH_DIR, "integrity_rate_of_month").

%% 通讯完整率
-define(USED_COMMU_OF_MONTH_DIR, "used_commu_of_month").

%% 使用量月份报表
-define(USED_REPORT_OF_MONTH_DIR, "used_report_of_month").


%%%====================================================
%% End 数据存储相关的的宏定义
%%%====================================================


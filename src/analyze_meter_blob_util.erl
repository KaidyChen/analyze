-module(analyze_meter_blob_util).

-compile(export_all).

-include("analyze.hrl").
-include("analyze_config.hrl").
-include("analyze_meter.hrl").

%% 获取能源使用量
get_energy(#blob{info = Info} = Meter_blob) when is_record(Info, ac_info) ->
    to_float(Info#ac_info.electric_power);
get_energy(#blob{info = Info} = Meter_blob) when is_record(Info, socket_info) ->
    to_float(Info#socket_info.electric_power);
get_energy(#blob{info = Info} = Meter_blob) when is_record(Info, central_ac_info) ->
    to_float(Info#central_ac_info.electric_power);
get_energy(_) ->
    0.0.

%% 上报时间
get_report_time(Meter_blob) ->
    Meter_blob#blob.report_time.

%% 电量
get_electric_power_float(#blob{info = Info} = Meter_blob) when is_record(Info, ac_info) ->
    to_float(Info#ac_info.electric_power);
get_electric_power_float(#blob{info = Info} = Meter_blob) when is_record(Info, socket_info) ->
    to_float(Info#socket_info.electric_power);
get_electric_power_float(#blob{info = Info} = Meter_blob) when is_record(Info, central_ac_info) ->
    to_float(Info#central_ac_info.electric_power);
get_electric_power_float(#blob{info = Info} = Meter_blob) when is_record(Info, three_phase_info) ->
    to_float(Info#three_phase_info.electric_power).


get_electric_power_str(#blob{info = Info} = Meter_blob) when is_record(Info, ac_info) ->
    to_str(Info#ac_info.electric_power);
get_electric_power_str(#blob{info = Info} = Meter_blob) when is_record(Info, socket_info) ->
    to_str(Info#socket_info.electric_power);
get_electric_power_str(#blob{info = Info} = Meter_blob) when is_record(Info, three_phase_info) ->
    to_str(Info#three_phase_info.electric_power);
get_electric_power_str(#blob{info = Info} = Meter_blob) when is_record(Info, central_ac_info) ->
    to_str(Info#central_ac_info.electric_power).


%% 功率
get_active_power_float(#blob{info = Info} = Meter_blob) when is_record(Info, ac_info) ->
    to_float(Info#ac_info.active_power);
get_active_power_float(#blob{info = Info} = Meter_blob) when is_record(Info, socket_info) ->
    to_float(Info#socket_info.active_power);
get_active_power_float(#blob{info = Info} = Meter_blob) when is_record(Info, central_ac_info) ->
    to_float(Info#central_ac_info.active_power).

get_active_power_str(#blob{info = Info} = Meter_blob) when is_record(Info, ac_info) ->
    binary_to_list(Info#ac_info.active_power);
get_active_power_str(#blob{info = Info} = Meter_blob) when is_record(Info, socket_info) ->
    binary_to_list(Info#socket_info.active_power);
get_active_power_str(#blob{info = Info} = Meter_blob) when is_record(Info, central_ac_info) ->
    binary_to_list(Info#central_ac_info.active_power).

%% 温度
get_temp_str(#blob{info = Info} = Meter_blob) when is_record(Info, ac_info) ->
    binary_to_list(Info#ac_info.temp);
get_temp_str(#blob{info = Info} = Meter_blob) when is_record(Info, central_ac_info) ->
    binary_to_list(Info#central_ac_info.temp).

%% 设备时标
get_datetime(#blob{info = Info} = Meter_blob) when is_record(Info, ac_info) ->
    Info#ac_info.datetime;
get_datetime(#blob{info = Info} = Meter_blob) when is_record(Info, socket_info) ->
    Info#socket_info.datetime;
get_datetime(#blob{info = Info} = Meter_blob) when is_record(Info, central_ac_info) ->
    Info#central_ac_info.datetime;
get_datetime(#blob{info = Info} = Meter_blob) when is_record(Info, three_phase_info) ->
    Info#three_phase_info.datetime;
get_datetime(#blob{info = Info} = Meter_blob) when is_record(Info, air_detector_info) ->
    Info#air_detector_info.datetime.

%% 继电器状态
get_relay_status_str(#blob{info = Info} = Meter_blob) when is_record(Info, ac_info) ->
    binary_to_list(Info#ac_info.relay_status);
get_relay_status_str(#blob{info = Info} = Meter_blob) when is_record(Info, socket_info) ->
    binary_to_list(Info#socket_info.relay_status);
get_relay_status_str(#blob{info = Info} = Meter_blob) when is_record(Info, central_ac_info) ->
    binary_to_list(Info#central_ac_info.relay_status).

%% 状态字
get_status_word(#blob{info = Info} = Meter_blob) when is_record(Info, ac_info) ->
    Info#ac_info.status_word;
get_status_word(#blob{info = Info} = Meter_blob) when is_record(Info, socket_info) ->
    Info#socket_info.status_word;
get_status_word(#blob{info = Info} = Meter_blob) when is_record(Info, central_ac_info) ->
    Info#central_ac_info.status_word;
get_status_word(_) ->       
    undefined.


%% 中央空调低中高速使用时长
get_low_medium_high_speed_used_time_str(#blob{info = Info} = Meter_blob) when is_record(Info, central_ac_info) ->
    {get_low_speed_used_time_str(Meter_blob), get_medium_speed_used_time_str(Meter_blob), get_high_speed_used_time_str(Meter_blob)}.

%% 中央空调低中高速总使用时长
get_low_medium_high_speed_used_time_sum(#blob{info = Info} = Meter_blob) when is_record(Info, central_ac_info) ->
    to_int(get_low_speed_used_time_str(Meter_blob)) + to_int(get_medium_speed_used_time_str(Meter_blob)) + to_int(get_high_speed_used_time_str(Meter_blob)).

    
%% 中央空调低速使用时长
get_low_speed_used_time_str(#blob{info = Info} = Meter_blob) when is_record(Info, central_ac_info) ->
    binary_to_list(Info#central_ac_info.low_speed_used_time).

%% 中央空调中速使用时长
get_medium_speed_used_time_str(#blob{info = Info} = Meter_blob) when is_record(Info, central_ac_info) ->
    binary_to_list(Info#central_ac_info.medium_speed_used_time).

%% 中央空调高速使用时长
get_high_speed_used_time_str(#blob{info = Info} = Meter_blob) when is_record(Info, central_ac_info) ->
    binary_to_list(Info#central_ac_info.high_speed_used_time).


%% PM1.0 
get_pm1dot0_str(#blob{info = Info} = Meter_blob) when is_record (Info, air_detector_info) ->
    binary_to_list(Info#air_detector_info.pm1dot0).

%% PM2.5
get_pm2dot5_str(#blob{info = Info} = Meter_blob) when is_record (Info, air_detector_info) ->
    binary_to_list(Info#air_detector_info.pm2dot5).

%% PM10
get_pm10_str(#blob{info = Info} = Meter_blob) when is_record (Info, air_detector_info) ->
    binary_to_list(Info#air_detector_info.pm10).

%% 温度
get_temperature_str(#blob{info = Info} = Meter_blob) when is_record (Info, air_detector_info) ->
    binary_to_list(Info#air_detector_info.temperature).

%% 湿度
get_humidity_str(#blob{info = Info} = Meter_blob) when is_record (Info, air_detector_info) ->
    binary_to_list(Info#air_detector_info.humidity).

%% 甲醛浓度
get_hcho_str(#blob{info = Info} = Meter_blob) when is_record (Info, air_detector_info) ->
    binary_to_list(Info#air_detector_info.hcho).

%% TVOC
get_tvoc_str(#blob{info = Info} = Meter_blob) when is_record (Info, air_detector_info) ->
    binary_to_list(Info#air_detector_info.hcho).

%% CO2
get_co2_str(#blob{info = Info} = Meter_blob) when is_record (Info, air_detector_info) ->
    binary_to_list(Info#air_detector_info.co2).

%% CO
get_co_str(#blob{info = Info} = Meter_blob) when is_record (Info, air_detector_info) ->
    binary_to_list(Info#air_detector_info.co).

%% o2
get_o2_str(#blob{info = Info} = Meter_blob) when is_record (Info, air_detector_info) ->
    binary_to_list(Info#air_detector_info.o2).

%% blob字段字符串

%% 分体空调
get_blob_field_str(#blob{info = Info} = Meter_blob) when is_record(Info, ac_info) ->
    Report_time_str = ?HELP:getDateTimeStr(Meter_blob#blob.report_time), 
    #ac_info{
        electric_power = Electric_power,        
        voltage = Voltage,                
        electric_current = Electric_current,       
        active_power = Active_power,           
        temp = Temp,                   
        power_system_frequency = Power_system_frequency, 
        power_factor = Power_factor,           
        relay_status = Relay_status,           
        status_word = Status_word,            
        datetime = Datetime            
    } = Info,
    Electric_power_str = to_str(Electric_power),
    Voltage_str = to_str(Voltage),
    Electric_current_str = to_str(Electric_current),
    Active_power_str = to_str(Active_power),
    Temp_str = to_str(Temp),
    Power_system_frequency_str = to_str(Power_system_frequency),
    Power_factor_str = to_str(Power_factor),
    Relay_status_str = to_str(Relay_status),
    Status_word_str = to_str(Status_word),
    Dev_datetime_str = ?HELP:getDateTimeStr(Datetime),
    list_to_binary(string:join([Report_time_str, Electric_power_str, Voltage_str, Electric_current_str, Active_power_str, Temp_str, Power_system_frequency_str, Power_factor_str, Relay_status_str, Status_word_str, Dev_datetime_str], "#"));
%% 中央空调
get_blob_field_str(#blob{info = Info} = Meter_blob) when is_record(Info, central_ac_info) ->
    Report_time_str = ?HELP:getDateTimeStr(Meter_blob#blob.report_time), 
    #central_ac_info{
        electric_power = Electric_power,                 % 当前组合有功总电能
        active_power = Active_power,                     % 功率
        temp = Temp,                                     % 环境温度
        relay_status = Relay_status,                     % 继电器状态
        temp_and_mode = Temp_and_mode,                   % 设定温度及模式
        wind_speed_gears = Wind_speed_gears,             % 风速档位
        low_speed_used_time = Low_speed_used_time,       % 低速累计使用时长
        medium_speed_used_time = Medium_speed_used_time, % 中速累计使用时长
        high_speed_used_time = High_speed_used_time,     % 高速累计使用时长
        amount = Amount,                                 % 剩余金额
        status_word = Status_word,                       % 状态字
        datetime = Datetime                              % 设备的时标    
    } = Info,
    Electric_power_str = to_str(Electric_power),
    Active_power_str = to_str(Active_power),
    Temp_str = to_str(Temp),
    Relay_status_str = to_str(Relay_status),
    Temp_and_mode_str = to_str(Temp_and_mode),
    Wind_speed_gears_str = to_str(Wind_speed_gears),
    Low_speed_used_time_str = to_str(Low_speed_used_time),
    Medium_speed_used_time_str = to_str(Medium_speed_used_time),
    High_speed_used_time_str = to_str(High_speed_used_time),
    Amount_str = to_str(Amount),
    Status_word_str = to_str(Status_word),
    Dev_datetime_str = ?HELP:getDateTimeStr(Datetime),
    list_to_binary(string:join([Report_time_str, Electric_power_str, Active_power_str, Temp_str, Relay_status_str, Temp_and_mode_str, Wind_speed_gears_str, Low_speed_used_time_str, Medium_speed_used_time_str, High_speed_used_time_str, Amount_str, Status_word_str, Dev_datetime_str], "#"));
%% 插座
get_blob_field_str(#blob{info = Info} = Meter_blob) when is_record(Info, socket_info) ->
    Report_time_str = ?HELP:getDateTimeStr(Meter_blob#blob.report_time), 
    #socket_info{
        electric_power = Electric_power,        
        voltage = Voltage,                
        electric_current = Electric_current,       
        active_power = Active_power,                           
        power_system_frequency = Power_system_frequency, 
        power_factor = Power_factor,           
        relay_status = Relay_status,           
        status_word = Status_word,            
        datetime = Datetime            
    } = Info,
    Electric_power_str = to_str(Electric_power),
    Voltage_str = to_str(Voltage),
    Electric_current_str = to_str(Electric_current),
    Active_power_str = to_str(Active_power),
    Power_system_frequency_str = to_str(Power_system_frequency),
    Power_factor_str = to_str(Power_factor),
    Relay_status_str = to_str(Relay_status),
    Status_word_str = to_str(Status_word),
    Dev_datetime_str = ?HELP:getDateTimeStr(Datetime),
    list_to_binary(string:join([Report_time_str, Electric_power_str, Voltage_str, Electric_current_str, Active_power_str, Power_system_frequency_str, Power_factor_str, Relay_status_str, Status_word_str, Dev_datetime_str], "#")).


to_float(Bitstring) ->
    ?HELP:to_float(Bitstring).

to_str(Bitstring) when is_binary(Bitstring) ->
    binary_to_list(Bitstring).

to_int(Str) ->
    list_to_integer(Str).







-module(analyze_report_data).

-include("analyze.hrl").
-include("analyze_config.hrl").
-include("print.hrl").
-include("analyze_meter.hrl").
-include("report.hrl").

-export([report_data/3]).

%%==================================================================================
%% External API
%%==================================================================================

report_data(Msg_type = ?DATAMSG, Data_field_str, Now_datetime) ->
    process_datamsg(Msg_type, Data_field_str, Now_datetime);
report_data(Msg_type = ?CONTROLMSG, Data_field_str, Now_datetime) ->
    process_controlmsg(Msg_type, Data_field_str, Now_datetime);
report_data(Msg_type = ?STATUSMSG, Data_field_str, Now_datetime) ->
    process_statusmsg(Msg_type, Data_field_str, Now_datetime);
report_data(Msg_type = ?WARNMSG, Data_field_str, Now_datetime) ->
    process_warnmsg(Msg_type, Data_field_str, Now_datetime).

%%==================================================================================
%% Internal API
%%==================================================================================

%% 处理数据块上报信息
process_datamsg(Msg_type, Data_field_str, Now_datetime) ->
    case parse_data_field(Data_field_str) of
        [Meter_type, Meter, Datagram] ->
            case parse_datagram(Datagram) of
                {true, report, Datetime, EnergyFloat} ->
                    ?PRINT("MeterId:~p Datetime:~p EnergyStr:~p~n", [Meter, Datetime, EnergyFloat]),

                    Avg_interval = 22,
                    case analyze_meter_util:is_on_the_hour(Datetime, Avg_interval) of
                        true ->
                            analyze_push_data_server:push_hour_data(Meter, Datetime, EnergyFloat);
                        false ->
                            ok
                    end,
                                                            
                    Info = #three_phase_info{electric_power = to_binary(?HELP:float_to_decimal_str(EnergyFloat, 2))},
                    Meter_blob = #blob{report_time = Now_datetime, info = Info},
                    ?PRINT("Meter_blob:~p~n", [Meter_blob]),
                    notify_meter_report_come(Msg_type, Meter_type, Meter, Meter_blob),
                    ok;
                ok ->
                    Info = undefined,
                    Meter_blob = #blob{report_time = Now_datetime, info = Info},
                    notify_meter_report_come(Msg_type, Meter_type, Meter, Meter_blob),
                    ok;
                %% 上报报文中各个字段信息
                %% Content: "电量 电压 电流 。。。"
                %% Info: record(ac_info) || record(socket_info)
                {Content, Info} ->
                    %% 通知虚拟设备上报到来
                    Meter_blob = #blob{report_time = Now_datetime, info = Info},
                    notify_meter_report_come(Msg_type, Meter_type, Meter, Meter_blob),
                    
                    Meter_datamsg_filepath = get_meter_datamsg_filepath(Meter, Now_datetime),
                    save_datamsg(Meter_datamsg_filepath, Content, Now_datetime),
                    ok;
                   
                false -> ?ERROR("parse_datagram is error:~p", [Data_field_str])
            end;
        _ -> 
            ?ERROR("Msg_type:~p data field:~p not match.~n", [Msg_type, Data_field_str])
    end.

process_controlmsg(Msg_type, Data_field_str, Now_datetime) ->
    ?PRINT("~p/~p~n", [Msg_type, Data_field_str]),
    case parse_data_field(Data_field_str) of
        [Meter_type, Meter, Datagram] ->
            Datagram_bin = ?HELP:string_to_binary(Datagram),
            case Datagram_bin of
                %% 触摸开关策略控制上报
                <<Id:8, 16#3B, 16#0F, 16#EF, 16#25, TaskIdBinary:16/binary-unit:8, _/binary>> when 
                      (Id =:= 16#34); (Id =:= 16#35); (Id =:= 16#36); (Id =:= 16#37) ->
                    TaskId = ?CALHELP:get_touch_task_id(TaskIdBinary),
                    catch analyze_lighting_strategy_server:touch_lighting_strategy(TaskId),
                    %%analyze_strategy_task_server:touch_strategy_task(TaskId),
                    ?PRINT("Touch taskId:~p~n", [TaskId]),
                    ok;
                %% 灯的控制聚合上报
                <<16#39, 16#3A, 16#0F, 16#EF, _Len1Tmp:8, NumberTmp:8, Rest/binary>> ->
                    %_Number = NumberTmp - 51,
                    ShortNumberAndCtrlStatus = get_short_number_and_ctrl_status(Rest),
                    ?PRINT("ShortNumberAndCtrlStatus:~p~n", [ShortNumberAndCtrlStatus]),
                    ok;
                %% 灯的控制上报
                <<16#35, 16#36, 16#0F, 16#EF, RelayStatusAndPwmSelect:8, Pwm:8/binary-unit:8, ActivePowerTmp:(3*8),
                  CtrlStatusTmp:8>> ->
                    <<_Retain:3, ReplyStatusInt:1, _PwmSelete:4>> = <<(RelayStatusAndPwmSelect-51):8>>,                   
                    ActivePowerFloat = ?CALHELP:get_active_power_float(ActivePowerTmp),
                    OnOffStatus = 
                        case {ReplyStatusInt, ActivePowerFloat >= 5.0} of
                            {1, true} ->  %% 继电器状态1为合闸
                                1;
                            _ ->
                                0
                        end,
                        %% 0 表示灯灭，1 表示灯亮, 灯的不是表示继电器状态                    
                    analyze_meter_on_off_status:insert(Meter_type, Meter, OnOffStatus),                    
                    CtrlStatus = CtrlStatusTmp - 51,
                    ?PRINT("~p/~p ReplyStatusInt:~p ActicvePower:~p CtrlStatus:~p OnOffStatus:~p~n", 
                           [Meter_type, Meter, ReplyStatusInt, ActivePowerFloat, CtrlStatus, OnOffStatus]),
                    ok;
                _ ->
                    ok
            end,
            ok;
        _ ->
            ?ERROR("Msg_type:~p data field:~p not match.~n", [Msg_type, Data_field_str])
    end.

get_short_number_and_ctrl_status(Rest) ->
    get_short_number_and_ctrl_status_(Rest, []).

get_short_number_and_ctrl_status_(<<ShortNumberTmp, CtrlStatusTmp, Rest/binary>>, List) ->
    get_short_number_and_ctrl_status_(Rest, [{ShortNumberTmp-51, CtrlStatusTmp-51} | List]);
get_short_number_and_ctrl_status_(_, List) ->
    List.

process_statusmsg(Msg_type, Data_field_str, Now_datetime) ->
    case parse_data_field(Data_field_str) of
        [Gateway_type, Gateway, Status] ->
            ?PRINT("Gateway_type:~p Gateway:~p Status:~p~n", [Gateway_type, Gateway, Status]),
            notify_gateway_report_come(Msg_type, Gateway_type, Gateway, Status);
        _ -> 
            ?ERROR("Msg_type:~p data field:~p not match.~n", [Msg_type, Data_field_str])
    end.

process_warnmsg(Msg_type, Data_field_str, Now_datetime) ->
    ?ERROR("~p: ~p", [Msg_type, Data_field_str]),
    ok.
    
parse_data_field(Data_field_str) ->
    string:tokens(Data_field_str, "#").

%% 解析数据报
parse_datagram(Datagram) ->
    Datagram_bin = ?HELP:string_to_binary(Datagram),
    case Datagram_bin of
        <<16#32, 16#33, 16#21, 16#ef, PM1dot0:(2*8), PM2dot5:(2*8), PM10:(2*8), Temperature:(2*8), Humidity:(1*8), HCHO:(2*8),
          TVOC:(2*8), CO2:(2*8), CO:(2*8), O2:(2*8), _Reserved:(2*8), DateTime:(5*8)>> ->
            {Content, Info} = get_air_detector_content_and_info({PM1dot0, PM2dot5, PM10, Temperature, Humidity, HCHO, TVOC,
                                                                CO2, CO, O2, DateTime}),
            {Content, Info};
        %% 分体空调
        <<16#38, 16#32, 16#ff, 16#ef, Electric_power:(4*8), Voltage:(2*8), Electric_current:(3*8), Active_power:(3*8), Temp:(2*8), _:8, Power_system_frequency:(2*8), Power_factor:(2*8), Relay_status:8, Status_word:(3*8), DateTime:(5*8)>> ->
            {Content, Info} = get_ac_content_and_info({Electric_power, Voltage, Electric_current, Active_power, Temp, Power_system_frequency, Power_factor, Relay_status, Status_word, DateTime}),
            {Content, Info};

        %% 插座或四路开关面板不含状态字和时标
        <<16#34, 16#32, 16#ff, 16#ef, Electric_power:(4*8), Voltage:(2*8), Electric_current:(3*8), Active_power:(3*8), _:(3*8), Power_system_frequency:(2*8), Power_factor:(2*8), Relay_status:8>> ->
            {Content, Info} = get_socket_content_and_info_1({Electric_power, Voltage, Electric_current, Active_power, Power_system_frequency, Power_factor, Relay_status}),
            {Content, Info};

        %% 插座或四路开关面板含有状态字和时标    
        <<16#34, 16#32, 16#ff, 16#ef, Electric_power:(4*8), Voltage:(2*8), Electric_current:(3*8), Active_power:(3*8), _:(3*8), Power_system_frequency:(2*8), Power_factor:(2*8), Relay_status:8, Status_word:(3*8), DateTime:(5*8)>> ->
            {Content, Info} = get_socket_content_and_info({Electric_power, Voltage, Electric_current, Active_power, Power_system_frequency, Power_factor, Relay_status, Status_word, DateTime}),
            {Content, Info};

        %% 中央空调
        %% 3832ddefb76b3433333333b835343359366a5b343354b5333389a83633b8aa333833333347475b3c49be16
        %%  
        % electric_power,         % 当前组合有功总电能
        % active_power,           % 功率
        % temp,                   % 环境温度
        % relay_status,           % 继电器状态
        % temp_and_mode           % 设定温度及模式
        % wind_speed_gears,       % 风速档位
        % low_speed_used_time,    % 低速累计使用时长
        % medium_speed_used_time, % 中速累计使用时长
        % high_speed_used_time,   % 高速累计使用时长
        % amount,                 % 剩余金额
        % status_word,            % 状态字
        % datetime,               % 设备的时标
        <<16#38, 16#32, 16#dd, 16#ef, Electric_power:(4*8), Active_power:(3*8), Temp:(2*8), Relay_status:8, Temp_and_mode:(2*8), Wind_speed_gears:8, Low_speed_used_time:(4*8), Medium_speed_used_time:(4*8), High_speed_used_time:(4*8), Amount:(4*8), Status_word:(3*8), DateTime:(5*8)>> ->
            {Content, Info} = get_central_ac_content_and_info({Electric_power, Active_power, Temp, Relay_status, Temp_and_mode, Wind_speed_gears, Low_speed_used_time, Medium_speed_used_time, High_speed_used_time, Amount, Status_word, DateTime}),
            {Content, Info};

        %% 水气表
        <<16#08, 16#17, MeterIdBinary:7/binary-unit:8, DatetimeBinary:5/binary-unit:8, Type:8, 
          AccumulativeVolumeBinary:5/binary-unit:8, _/binary>> when ((16#10 =< Type) andalso (Type =< 19)) 
                                                              orelse ((16#30 =< Type) andalso (Type =< 49)) ->
            
            try ?CALHELP:get_3761_accumulative_volume_float(AccumulativeVolumeBinary) of
                AccumulativeVolume ->
                    MeterId = ?CALHELP:get_3761_meter_id(MeterIdBinary),
                    Datetime = ?CALHELP:get_3761_date_time(DatetimeBinary),
                    AccumulativeVolumeStr = help:float_to_decimal_str(AccumulativeVolume, 2),
                    analyze_xml_report_server:report_data(MeterId, Datetime, 
                                                          [{?TOTAL_WATER_CONSUMPTION, AccumulativeVolumeStr}]),
                    {true, report, Datetime, AccumulativeVolume}
            catch
                _:_ ->
                    ok
            end;
        %% 热表
        <<16#08, 16#17, MeterIdBinary:7/binary-unit:8, DatetimeBinary:5/binary-unit:8, Type:8, 
          _DayCaloriesBinary:5/binary-unit:8, TotalCaloriesBinary:5/binary-unit:8, _/binary>> when 
              ((16#20 =< Type) andalso (Type =< 29)) ->
            Datetime = ?CALHELP:get_3761_date_time(DatetimeBinary),
            try ?CALHELP:get_3761_calories_float(TotalCaloriesBinary) of
                TotalCalories ->
                    {true, report, Datetime, TotalCalories}
            catch
                _:_ ->
                    ok
            end;
        <<16#01, 16#10, _/binary>> ->
            cal_dnb_data(Datagram_bin);
        <<16#04, 16#10, _/binary>> ->
            cal_dnb_data(Datagram_bin);
        <<16#01, 16#03, _/binary>> ->
            cal_dnb_data(Datagram_bin);
        Other ->
            false 
    end.

cal_dnb_data(Packet) ->
    ?PRINT("Packet:~p~n", [hex_util:to_hex(Packet)]),
    case Packet of
        %% 电表数据F25，各项功率数据
        <<16#01, 16#03, MeterIdBinary:7/binary-unit:8, DatetimeBinary:5/binary-unit:8, TotalActivePower:3/binary-unit:8, 
          AActivePower:3/binary-unit:8, BActivePower:3/binary-unit:8, CActivePower:3/binary-unit:8, 
          TotalReactivePower:3/binary-unit:8, AReactivePower:3/binary-unit:8, BReactivePower:3/binary-unit:8, 
          CReactivePower:3/binary-unit:8, TotalPowerFactor:2/binary-unit:8, APowerFactor:2/binary-unit:8, 
          BPowerFactor:2/binary-unit:8, CPowerFactor:2/binary-unit:8, AVoltage:2/binary-unit:8, BVoltage:2/binary-unit:8, 
          CVoltage:2/binary-unit:8, ACurrent:3/binary-unit:8, BCurrent:3/binary-unit:8, CCurrent:3/binary-unit:8, 
          ZeroCurrent:3/binary-unit:8, TotalApparentPower:3/binary-unit:8, AApparentPower:3/binary-unit:8, 
          BApparentPower:3/binary-unit:8, CApparentPower:3/binary-unit:8, _:16, Rest/binary>> ->
            MeterId = ?CALHELP:get_3761_meter_id(MeterIdBinary),
            Datetime = ?CALHELP:get_3761_date_time(DatetimeBinary),
            TotalActivePowerStr = ?CALHELP:get_3761_active_power(TotalActivePower),
            AActivePowerStr = ?CALHELP:get_3761_active_power(AActivePower),
            BActivePowerStr = ?CALHELP:get_3761_active_power(BActivePower),
            CActivePowerStr = ?CALHELP:get_3761_active_power(CActivePower),            
            TotalReactivePowerStr = ?CALHELP:get_3761_active_power(TotalReactivePower),
            AReactivePowerStr = ?CALHELP:get_3761_active_power(AReactivePower),
            BReactivePowerStr = ?CALHELP:get_3761_active_power(BReactivePower),
            CReactivePowerStr = ?CALHELP:get_3761_active_power(CReactivePower),
            TotalPowerFactorStr = ?CALHELP:get_3761_power_factor(TotalPowerFactor),
            APowerFactorStr = ?CALHELP:get_3761_power_factor(APowerFactor),
            BPowerFactorStr = ?CALHELP:get_3761_power_factor(BPowerFactor),
            CPowerFactorStr = ?CALHELP:get_3761_power_factor(CPowerFactor),
            AVoltageStr = ?CALHELP:get_3761_voltage(AVoltage),
            BVoltageStr = ?CALHELP:get_3761_voltage(BVoltage),
            CVoltageStr = ?CALHELP:get_3761_voltage(CVoltage),
            ACurrentStr = ?CALHELP:get_3761_current(ACurrent),
            BCurrentStr = ?CALHELP:get_3761_current(BCurrent),
            CCurrentStr = ?CALHELP:get_3761_current(CCurrent),
            ZeroCurrentStr = ?CALHELP:get_3761_current(ZeroCurrent),
            TotalApparentPowerStr = ?CALHELP:get_3761_apparent_power(TotalApparentPower),
            AApparentPowerStr = ?CALHELP:get_3761_apparent_power(AApparentPower),
            BApparentPowerStr = ?CALHELP:get_3761_apparent_power(BApparentPower),
            CApparentPowerStr = ?CALHELP:get_3761_apparent_power(CApparentPower),
            
            DataItemMultiList = [
                                 [{?TOTAL_ACTIVE_POWER, TotalActivePowerStr}, {?A_ACTIVE_POWER, AActivePowerStr},
                                  {?B_ACTIVE_POWER, BActivePowerStr}, {?C_ACTIVE_POWER, CActivePowerStr},
                                  {?TOTAL_REACTIVE_POWER, TotalReactivePowerStr}, {?A_REACTIVE_POWER, AReactivePowerStr},
                                  {?B_REACTIVE_POWER, BReactivePowerStr}, {?C_REACTIVE_POWER, CReactivePowerStr},
                                  {?TOTAL_POWER_FACTOR, TotalPowerFactorStr}, {?A_POWER_FACTOR, APowerFactorStr},
                                  {?B_POWER_FACTOR, BPowerFactorStr}, {?C_POWER_FACTOR, CPowerFactorStr}
                                 ],
                                 [{?A_VOLTAGE, AVoltageStr}, {?B_VOLTAGE, BVoltageStr}, {?C_VOLTAGE, CVoltageStr},
                                  {?A_CURRENT, ACurrentStr}, {?B_CURRENT, BCurrentStr}, {?C_CURRENT, CCurrentStr},
                                  {?ZERO_CURRENT, ZeroCurrentStr}, {?TOTAL_APPARENT_POWER, TotalApparentPowerStr}, 
                                  {?A_APPARENT_POWER, AApparentPowerStr}, {?B_APPARENT_POWER, BApparentPowerStr}, 
                                  {?C_APPARENT_POWER, CApparentPowerStr}
                                 ]
                                ],
                
            [analyze_xml_report_server:report_data(MeterId, Datetime, DataItemList) || DataItemList <- DataItemMultiList],         
            cal_dnb_data(Rest);
        
        <<16#01, 16#10, MeterIdBinary:7/binary-unit:8, DatetimeBinary:5/binary-unit:8, RateNumber:8, 
          PositiveActiveEle:5/binary-unit:8, Rate1PositiveActiveEle:5/binary-unit:8, Rate2PositiveActiveEle:5/binary-unit:8,
          Rate3PositiveActiveEle:5/binary-unit:8, Rate4PositiveActiveEle:5/binary-unit:8, _:2/binary-unit:8, 
          16#04, 16#10, MeterIdBinary:7/binary-unit:8, _DatetimeBinary:5/binary-unit:8, _RateNumber:8, 
          InverseActiveEle:5/binary-unit:8, Rate1InverseActiveEle:5/binary-unit:8, Rate2InverseActiveEle:5/binary-unit:8,
          Rate3InverseActiveEle:5/binary-unit:8, Rate4InverseActiveEle:5/binary-unit:8, _/binary>> ->
            MeterId = ?CALHELP:get_3761_meter_id(MeterIdBinary),
            Datetime = ?CALHELP:get_3761_date_time(DatetimeBinary),
            PositiveActiveEleStr = ?CALHELP:get_3761_active_electricity_str(PositiveActiveEle),
            Rate1PositiveActiveEleStr = ?CALHELP:get_3761_active_electricity_str(Rate1PositiveActiveEle),
            Rate2PositiveActiveEleStr = ?CALHELP:get_3761_active_electricity_str(Rate2PositiveActiveEle),
            Rate3PositiveActiveEleStr = ?CALHELP:get_3761_active_electricity_str(Rate3PositiveActiveEle),
            Rate4PositiveActiveEleStr = ?CALHELP:get_3761_active_electricity_str(Rate4PositiveActiveEle),
            InverseActiveEleStr = ?CALHELP:get_3761_active_electricity_str(InverseActiveEle),
            Rate1InverseActiveEleStr = ?CALHELP:get_3761_active_electricity_str(Rate1InverseActiveEle),
            Rate2InverseActiveEleStr = ?CALHELP:get_3761_active_electricity_str(Rate2InverseActiveEle),
            Rate3InverseActiveEleStr = ?CALHELP:get_3761_active_electricity_str(Rate3InverseActiveEle),
            Rate4InverseActiveEleStr = ?CALHELP:get_3761_active_electricity_str(Rate4InverseActiveEle),
            
            DataItemMultiList = [
                                 [{?POSITIVE_ACTIVE_ELE, PositiveActiveEleStr}, 
                                  {?RATE1_POSITIVE_ACTIVE_ELE, Rate1PositiveActiveEleStr},
                                  {?RATE2_POSITIVE_ACTIVE_ELE, Rate2PositiveActiveEleStr},
                                  {?RATE3_POSITIVE_ACTIVE_ELE, Rate3PositiveActiveEleStr},
                                  {?RATE4_POSITIVE_ACTIVE_ELE, Rate4PositiveActiveEleStr},
                                  {?INVERSE_ACTIVE_ELE, InverseActiveEleStr},
                                  {?RATE1_INVERSE_ACTIVE_ELE, Rate1InverseActiveEleStr},
                                  {?RATE2_INVERSE_ACTIVE_ELE, Rate2InverseActiveEleStr},
                                  {?RATE3_INVERSE_ACTIVE_ELE, Rate3InverseActiveEleStr},
                                  {?RATE4_INVERSE_ACTIVE_ELE, Rate4InverseActiveEleStr}
                                 ]
                                ],                
            [analyze_xml_report_server:report_data(MeterId, Datetime, DataItemList) || DataItemList <- DataItemMultiList],         

            PositiveActiveEleFloat = list_to_float(PositiveActiveEleStr),
            InverseActiveEleFloat = list_to_float(InverseActiveEleStr),        
            {true, report, Datetime, PositiveActiveEleFloat+InverseActiveEleFloat};
            
        %% 电表数据F129，各项总电能
        <<16#01, 16#10, MeterIdBinary:7/binary-unit:8, DatetimeBinary:5/binary-unit:8, RateNumber:8, 
          TotalActiveElectricityBinary:5/binary-unit:8, _/binary>> ->
            MeterId = ?CALHELP:get_3761_meter_id(MeterIdBinary),
            Datetime = ?CALHELP:get_3761_date_time(DatetimeBinary),
            TotalActiveElectricity = ?CALHELP:get_3761_active_electricity_float(TotalActiveElectricityBinary),
            {true, report, Datetime, TotalActiveElectricity};
        _ ->
            ok
    end.

get_air_detector_content_and_info({PM1dot0, PM2dot5, PM10, Temperature, Humidity, HCHO, TVOC,
                                   CO2, CO, O2, DateTime}) ->
    PM1dot0_str = ?CALHELP:get_pm1dot0_str(PM1dot0), 
    PM2dot5_str = ?CALHELP:get_pm2dot5_str(PM2dot5), 
    PM10_str = ?CALHELP:get_pm10_str(PM10), 
    Temperature_str = ?CALHELP:get_temperature_str(Temperature), 
    Humidity_str = ?CALHELP:get_humidity_str(Humidity), 
    HCHO_str = ?CALHELP:get_hcho_str(HCHO), 
    TVOC_str = ?CALHELP:get_tvoc_str(TVOC), 
    CO2_str = ?CALHELP:get_co2_str(CO2), 
    CO_str = ?CALHELP:get_co_str(CO), 
    O2_str = ?CALHELP:get_o2_str(O2),
    DateTime_tuple = ?CALHELP:get_datetime(DateTime),
    Content = string:join([PM1dot0_str, PM2dot5_str, PM10_str, Temperature_str, Humidity_str, HCHO_str, TVOC_str, CO2_str,
                          CO_str, O2_str, ?HELP:getDateTimeStr(DateTime_tuple)], ?FS),
    
    Air_detector_info = get_air_detector_info({PM1dot0_str, PM2dot5_str, PM10_str, Temperature_str, Humidity_str,
                                              HCHO_str, TVOC_str, CO2_str, CO_str, O2_str, DateTime_tuple}),
    {Content, Air_detector_info}.

get_air_detector_info({PM1dot0_str, PM2dot5_str, PM10_str, Temperature_str, Humidity_str,
                       HCHO_str, TVOC_str, CO2_str, CO_str, O2_str, DateTime_tuple}) ->
    Air_detector_info = 
        #air_detector_info{
           pm1dot0 = to_binary(PM1dot0_str),
           pm2dot5 = to_binary(PM2dot5_str),
           pm10 = to_binary(PM10_str),
           temperature = to_binary(Temperature_str),
           humidity = to_binary(Humidity_str),
           hcho = to_binary(HCHO_str),
           tvoc = to_binary(TVOC_str),
           co2 = to_binary(CO2_str),
           co = to_binary(CO_str),
           o2 = to_binary(O2_str),
           datetime = DateTime_tuple
          },
    Air_detector_info.
        

get_ac_content_and_info({Electric_power, Voltage, Electric_current, Active_power, Temp, Power_system_frequency, 
                         Power_factor, Relay_status, Status_word, DateTime}) ->
    {Electric_power_str, Voltage_str, Electric_current_str, Active_power_str, Power_system_frequency_str, 
     Power_factor_str, Relay_status_str, Status_word_str, DateTime_tuple} = 
        get_normal_info({Electric_power, Voltage, Electric_current, Active_power, Power_system_frequency, Power_factor, 
                         Relay_status, Status_word, DateTime}),
    Temp_str = ?CALHELP:get_temp(Temp),
    Content = string:join([Electric_power_str, Voltage_str, Electric_current_str, Active_power_str, Temp_str, Power_system_frequency_str, Power_factor_str, Relay_status_str, Status_word_str, ?HELP:getDateTimeStr(DateTime_tuple)], ?FS),
    Ac_info = get_ac_info({Electric_power_str, Voltage_str, Electric_current_str, Active_power_str, Temp_str, Power_system_frequency_str, Power_factor_str, Relay_status_str, Status_word_str, DateTime_tuple}),
    {Content, Ac_info}.

get_socket_content_and_info({Electric_power, Voltage, Electric_current, Active_power, Power_system_frequency, Power_factor, Relay_status, Status_word, DateTime}) ->
    {Electric_power_str, Voltage_str, Electric_current_str, Active_power_str, Power_system_frequency_str, Power_factor_str, Relay_status_str, Status_word_str, DateTime_tuple} = get_normal_info({Electric_power, Voltage, Electric_current, Active_power, Power_system_frequency, Power_factor, Relay_status, Status_word, DateTime}),
    Content = string:join([Electric_power_str, Voltage_str, Electric_current_str, Active_power_str, Power_system_frequency_str, Power_factor_str, Relay_status_str, Status_word_str, ?HELP:getDateTimeStr(DateTime_tuple)], ?FS),
    Socket_info = get_socket_info({Electric_power_str, Voltage_str, Electric_current_str, Active_power_str, Power_system_frequency_str, Power_factor_str, Relay_status_str, Status_word_str, DateTime_tuple}),
    {Content, Socket_info}.

get_central_ac_content_and_info({Electric_power, Active_power, Temp, Relay_status, Temp_and_mode, Wind_speed_gears, Low_speed_used_time, Medium_speed_used_time, High_speed_used_time, Amount, Status_word, DateTime}) ->
    Electric_power_str = ?CALHELP:get_electric_power(Electric_power),
    Active_power_str = ?CALHELP:get_active_power(Active_power),
    Temp_str = ?CALHELP:get_temp(Temp),
    Relay_status_str = ?CALHELP:get_relay_status(Relay_status),
    Temp_and_mode_str = ?CALHELP:get_temp_and_mode(Temp_and_mode),
    Wind_speed_gears_str = ?CALHELP:get_wind_speed_gears(Wind_speed_gears),
    Low_speed_used_time_str = ?CALHELP:get_xxx_speed_used_time(Low_speed_used_time),
    Medium_speed_used_time_str = ?CALHELP:get_xxx_speed_used_time(Medium_speed_used_time),
    High_speed_used_time_str = ?CALHELP:get_xxx_speed_used_time(High_speed_used_time),
    Amount_str = ?CALHELP:get_amount(Amount),
    Status_word_str = ?CALHELP:get_status_word(Status_word),
    DateTime_tuple = ?CALHELP:get_datetime(DateTime),
    Content = string:join([Electric_power_str, Active_power_str, Temp_str, Relay_status_str, Temp_and_mode_str, Wind_speed_gears_str, Low_speed_used_time_str, Medium_speed_used_time_str, High_speed_used_time_str, Amount_str, Status_word_str, ?HELP:getDateTimeStr(DateTime_tuple)], ?FS),
    Central_ac_info = get_central_ac_info({Electric_power_str, Active_power_str, Temp_str, Relay_status_str, Temp_and_mode_str, Wind_speed_gears_str, Low_speed_used_time_str, Medium_speed_used_time_str, High_speed_used_time_str, Amount_str, Status_word_str, DateTime_tuple}),
    {Content, Central_ac_info}.

%% 兼容上报没有状态字和时标的插座
get_socket_content_and_info_1({Electric_power, Voltage, Electric_current, Active_power, Power_system_frequency, Power_factor, Relay_status}) ->
    Electric_power_str = ?CALHELP:get_electric_power(Electric_power),
    Voltage_str = ?CALHELP:get_votage(Voltage),
    Electric_current_str = ?CALHELP:get_electric_current(Electric_current),
    Active_power_str = ?CALHELP:get_active_power(Active_power),
    Power_system_frequency_str = ?CALHELP:get_power_system_frequency(Power_system_frequency),
    Power_factor_str = ?CALHELP:get_power_factor(Power_factor),
    Relay_status_str = ?CALHELP:get_relay_status(Relay_status),
    Content = string:join([Electric_power_str, Voltage_str, Electric_current_str, Active_power_str, Power_system_frequency_str, Power_factor_str, Relay_status_str], ?FS),
    Socket_info = #socket_info{
        electric_power = to_binary(Electric_power_str), 
        voltage = to_binary(Voltage_str), 
        electric_current = to_binary(Electric_current_str), 
        active_power = to_binary(Active_power_str), 
        power_system_frequency = to_binary(Power_system_frequency_str), 
        power_factor = to_binary(Power_factor_str), 
        relay_status = to_binary(Relay_status_str),
        status_word = undefined, 
        datetime = undefined
    },
    {Content, Socket_info}.


%% 获取共有的属性信息
get_normal_info({Electric_power, Voltage, Electric_current, Active_power, Power_system_frequency, Power_factor, Relay_status, Status_word, DateTime}) ->
    Electric_power_str = ?CALHELP:get_electric_power(Electric_power),
    Voltage_str = ?CALHELP:get_votage(Voltage),
    Electric_current_str = ?CALHELP:get_electric_current(Electric_current),
    Active_power_str = ?CALHELP:get_active_power(Active_power),
    Power_system_frequency_str = ?CALHELP:get_power_system_frequency(Power_system_frequency),
    Power_factor_str = ?CALHELP:get_power_factor(Power_factor),
    Relay_status_str = ?CALHELP:get_relay_status(Relay_status),
    Status_word_str = ?CALHELP:get_status_word(Status_word),
    DateTime_tuple = ?CALHELP:get_datetime(DateTime),
    {Electric_power_str, Voltage_str, Electric_current_str, Active_power_str, Power_system_frequency_str, Power_factor_str, Relay_status_str, Status_word_str, DateTime_tuple}.

get_ac_info({Electric_power_str, Voltage_str, Electric_current_str, Active_power_str, Temp_str, Power_system_frequency_str, Power_factor_str, Relay_status_str, Status_word_str, DateTime_tuple}) -> 
    #ac_info{
        electric_power = to_binary(Electric_power_str), 
        voltage = to_binary(Voltage_str), 
        electric_current = to_binary(Electric_current_str), 
        active_power = to_binary(Active_power_str), 
        temp = to_binary(Temp_str), 
        power_system_frequency = to_binary(Power_system_frequency_str), 
        power_factor = to_binary(Power_factor_str), 
        relay_status = to_binary(Relay_status_str),
        status_word = to_binary(Status_word_str), 
        datetime = DateTime_tuple
    }.

get_central_ac_info({Electric_power_str, Active_power_str, Temp_str, Relay_status_str, Temp_and_mode_str, Wind_speed_gears_str, Low_speed_used_time_str, Medium_speed_used_time_str, High_speed_used_time_str, Amount_str, Status_word_str, DateTime_tuple}) ->
    #central_ac_info{
        electric_power = to_binary(Electric_power_str), 
        active_power = to_binary(Active_power_str), 
        temp = to_binary(Temp_str), 
        relay_status = to_binary(Relay_status_str),
        temp_and_mode = to_binary(Temp_and_mode_str),
        wind_speed_gears = to_binary(Wind_speed_gears_str), 
        low_speed_used_time = to_binary(Low_speed_used_time_str), 
        medium_speed_used_time = to_binary(Medium_speed_used_time_str), 
        high_speed_used_time = to_binary(High_speed_used_time_str), 
        amount = to_binary(Amount_str),
        status_word = to_binary(Status_word_str), 
        datetime = DateTime_tuple
    }.

get_socket_info({Electric_power_str, Voltage_str, Electric_current_str, Active_power_str, Power_system_frequency_str, Power_factor_str, Relay_status_str, Status_word_str, DateTime_tuple}) ->
    #socket_info{
        electric_power = to_binary(Electric_power_str), 
        voltage = to_binary(Voltage_str), 
        electric_current = to_binary(Electric_current_str), 
        active_power = to_binary(Active_power_str), 
        power_system_frequency = to_binary(Power_system_frequency_str), 
        power_factor = to_binary(Power_factor_str), 
        relay_status = to_binary(Relay_status_str),
        status_word = to_binary(Status_word_str), 
        datetime = DateTime_tuple
    }.
    
to_binary(String) when is_list(String) ->
    ?HELP:to_binary(String).

%%==================================================================================
%% Internal API
%%==================================================================================

%% 获取上报的报文要写入的文件路径
%% METERDIR/Meter/Year/Month/Year-Month-Day + SUFFIX
get_meter_datamsg_filepath(Meter, Now_datetime) ->
    analyze_report_util:get_meter_datamsg_filepath(Meter, Now_datetime).

%% 保存上报数据中各个属性的信息
%% Meter_datamsg_filepath:文件路径
%% Content: "电量 电压 电流 。。。"
%% Now_datetime: 上报到来的时间 {date, time}
save_datamsg(Meter_datamsg_filepath, Content, Now_datetime) ->
    filelib:ensure_dir(Meter_datamsg_filepath),
    case analyze_report_util:save_datetime_and_Content(Meter_datamsg_filepath, Content, Now_datetime) of
        ok ->
            ?PRINT("save_dataMsg Meter:~p is ok~n", [Meter_datamsg_filepath]),
            ok;
        {error, Reason} ->
            ?ERROR("save_datamsg ~p is error:~p", [Meter_datamsg_filepath, file:format_error(Reason)])
    end.

%% 通知虚拟设备上报到来
notify_meter_report_come(Msg_type, Meter_type, Meter, Meter_blob) ->
    case analyze_meter_util:get_running_pid(Meter_type, Meter) of
        {ok, Pid} ->
            Pid ! {report_come, Msg_type, Meter_type, Meter, Meter_blob};
        {error, Reason} ->
            ?ERROR("notify_meter_report_come Meter:~s is ~p~n", [Meter, Reason])
    end.

notify_gateway_report_come(Msg_type, Gateway_type, Gateway, Status) ->
    case analyze_gateway_pid:lookup(Gateway) of
        {ok, Pid} ->
            Pid ! {report_come, Msg_type, Gateway_type, Gateway, Status};
        {error, Reason} ->
            ?ERROR("notify_gateway_report_come Gateway:~s Msg_type:~s Status:~p is error:~p~n", [Gateway, Msg_type, Status, Reason])
    end.







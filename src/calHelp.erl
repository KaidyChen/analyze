-module(calHelp).

-include("analyze.hrl").
-include("analyze_config.hrl").

-compile(export_all).

getFilePathOfDay(Meter_Str, Date = {Year, Month, _Day}) ->
    Date_Str = ?HELP:dateToStr(Date, "-"),
    Year_Str = integer_to_list(Year),
    Month_Str = integer_to_list(Month),
    FileName = lists:concat([Date_Str, ?SUFFIX]),
    filename:join([?METERDIR, Meter_Str, Year_Str, Month_Str, FileName]).

getFilePathOfAvgHour(Meter_Str, Date) ->
    {Year, Month, _Day} = Date,
    Year_Str = integer_to_list(Year),
    Month_Str = integer_to_list(Month),
    FileName = lists:concat([Year_Str, "-", Month_Str, ?SUFFIX]),
    filename:join([?METERDIR, Meter_Str, ?AVGHOURDIR, FileName]).


getFileContentList(FilePath) ->
    case filelib:is_regular(FilePath) of
        true ->
            case file:read_file(FilePath) of
                {ok, Data_Bin} ->
                    Data_List = binary_to_list(Data_Bin),
                    DataLine_List = string:tokens(Data_List, ?NL),
                    {ok, DataLine_List};
                {error, _Reason} ->
                    {ok, []}
            end;       
        false ->
            {ok, []}
    end.

writeToFile(FilePath, Content) ->
    case file:open(FilePath, [write, binary]) of
        {ok, Fd} ->
            io:fwrite(Fd, "~s~n", [Content]),
            file:close(Fd),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.
 
%% 电能格式 XXXXXX.XX
get_electric_power(Electric_power) ->    
    Electric_power_tmp = lists:reverse(binary_to_list(<< <<X/integer>> || <<X>> <= <<Electric_power:(4*8)>> >>)),
    %% 对每个数据进行减去51(33H)，645协议规定， sub51函数，返回的是"XX"(16进制)
    Electric_power_tmp_List = [?HELP:sub51(X) || X <- Electric_power_tmp],
    %% 进行扁平化为"XXXXXXXX"
    Electric_power_List = lists:flatten(Electric_power_tmp_List),
    %% 获取整数部分(3byte, 6个字符), 并拼接成字符串
    Electric_power_List1 = lists:sublist(Electric_power_List, 1, 6),
    %% 获取小数部分(1byte, 2个字符)
    Electric_power_List2 = lists:sublist(Electric_power_List, 7, 2),
    %% 拼接成完整数据 "整数部分.小数部分"
    lists:concat([Electric_power_List1, ".", Electric_power_List2]).

%% 电压格式 XXX.X
get_votage(Votage) ->
    Votage_tmp = lists:reverse(binary_to_list(<< <<X/integer>> || <<X>> <= <<Votage:(2*8)>> >>)),
    Votage_tmp_List = [?HELP:sub51(X) || X <- Votage_tmp],
    %% 进行扁平化为"XXXX"
    Votage_List = lists:flatten(Votage_tmp_List),
    Votage_List_1 = lists:sublist(Votage_List, 1, 3),
    Votage_List_2 = lists:sublist(Votage_List, 4, 1),
    %% 拼接成完整数据 "整数部分.小数部分"
    lists:concat([Votage_List_1, ".", Votage_List_2]).

%% 电流格式 XXX.XXX
get_electric_current(Electric_current) ->
    Electric_current_tmp = lists:reverse(binary_to_list(<< <<X/integer>> || <<X>> <= <<Electric_current:(3*8)>> >>)),
    Electric_current_tmp_List = [?HELP:sub51(X) || X <- Electric_current_tmp],
    Electric_current_List = lists:flatten(Electric_current_tmp_List),
    Electric_current_List_1 = lists:sublist(Electric_current_List, 1, 3),
    Electric_current_List_2 = lists:sublist(Electric_current_List, 4, 3),
    lists:concat([Electric_current_List_1, ".", Electric_current_List_2]).

%% 总有功功率格式 XX.XXXX kw -> ???w
get_active_power(Active_power) ->
    %% 对每个数据域进行反转
    Active_power_tmp = lists:reverse(binary_to_list(<< <<X/integer>> || <<X>> <= <<Active_power:(3*8)>> >>)),
    %% 对每个数据进行减去51(33H)，645协议规定， sub51函数，返回的是"XX"(16进制)
    Active_power_tmp_List = [?HELP:sub51(X) || X <- Active_power_tmp],
    %% 进行扁平化为"XXXXXX"
    Active_power_List = lists:flatten(Active_power_tmp_List),
    %% 获取整数部分(1byte, 2个字符), 并拼接成字符串
    Active_power_List1 = lists:sublist(Active_power_List, 1, 2),
    %% 获取小数部分(2byte, 4个字符)
    Active_power_List2 = lists:sublist(Active_power_List, 3, 4),
    %% 拼接成完整数据 "整数部分.小数部分"
    Active_power_Str = lists:concat([Active_power_List1, ".", Active_power_List2]),
    %% 总有功功率单位是千瓦(kw), 需要转换为瓦(w), 保留两位小数
    Active_power_float = list_to_float(Active_power_Str) * 1000,
    float_to_list(Active_power_float, [{decimals, 2}, compact]).

get_active_power_float(Active_power) ->
    list_to_float(get_active_power(Active_power)).

%% 电网频率
get_power_system_frequency(Power_system_frequency) ->
    %% 对每个数据域进行反转
    Power_system_frequency_tmp = lists:reverse(binary_to_list(<< <<X/integer>> || <<X>> <= <<Power_system_frequency:(2*8)>> >>)),
    %% 对每个数据进行减去51(33H)，645协议规定， sub51函数，返回的是"XX"(16进制)
    Power_system_frequency_tmp_List = [?HELP:sub51(X) || X <- Power_system_frequency_tmp],
    %% 进行扁平化为"XXXX"
    Power_system_frequency_List = lists:flatten(Power_system_frequency_tmp_List),
    %% 获取整数部分(1byte, 2个字符), 并拼接成字符串
    Power_system_frequency_List_1 = lists:sublist(Power_system_frequency_List, 1, 2),
    %% 获取小数部分(1byte, 2个字符)
    Power_system_frequency_List_2 = lists:sublist(Power_system_frequency_List, 3, 2),
    %% 拼接成完整数据 "整数部分.小数部分"
    Power_system_frequency_Str = lists:concat([Power_system_frequency_List_1, ".", Power_system_frequency_List_2]),
    Power_system_frequency_Str.

%% 功率因子
get_power_factor(Power_factor) ->
    Power_factor_tmp = lists:reverse(binary_to_list(<< <<X/integer>> || <<X>> <= <<Power_factor:(2*8)>> >>)),
    Power_factor_tmp_List = [?HELP:sub51(X) || X <- Power_factor_tmp],
    Power_factor_List = lists:flatten(Power_factor_tmp_List),
    Power_factor_List_1 = lists:sublist(Power_factor_List, 1, 1),
    Power_factor_List_2 = lists:sublist(Power_factor_List, 2, 3),
    Power_factor_Str = lists:concat([Power_factor_List_1, ".", Power_factor_List_2]),
    Power_factor_Str.

%% 继电器状态
get_relay_status(Relay_status) ->
    Relay_status_tmp = binary_to_list(<< <<X/integer>> || <<X>> <= <<Relay_status:(1*8)>> >>),
    Relay_status_tmp_List = [?HELP:sub51(X) || X <- Relay_status_tmp],
    lists:flatten(Relay_status_tmp_List).

%% 状态字
get_status_word(Status_word) ->
    Status_word_tmp = lists:reverse(binary_to_list(<< <<X/integer>> || <<X>> <= <<Status_word:(3*8)>> >>)),
    Status_word_tmp_List = [?HELP:sub51(X) || X <- Status_word_tmp],
    Status_word_Str = lists:flatten(Status_word_tmp_List),
    Status_word_Str.

%% 温度格式 XXX.X
get_temp(Temp) ->
    Temp_tmp = lists:reverse(binary_to_list(<< <<X/integer>> || <<X>> <= <<Temp:(2*8)>> >>)),
    Temp_tmp_List = [?HELP:sub51(X) || X <- Temp_tmp],
    Temp_List = lists:flatten(Temp_tmp_List),
    Temp_List_1 = lists:sublist(Temp_List, 1, 3),
    Temp_List_2 = lists:sublist(Temp_List, 4, 1),
    lists:concat([Temp_List_1, ".", Temp_List_2]).

%% YYMMDDHHmm
get_datetime(DateTime) ->
    DateTime_tmp = lists:reverse(binary_to_list(<< <<X/integer>> || <<X>> <= <<DateTime:(5*8)>> >>)),
    DateTime_tmp_List = [?HELP:sub51(X) || X <- DateTime_tmp],
    DateTime_List = lists:flatten(DateTime_tmp_List),
    DateTime_Year = list_to_integer(lists:sublist(DateTime_List, 1, 2)) + 2000,
    DateTime_Month = list_to_integer(lists:sublist(DateTime_List, 3, 2)),
    DateTime_Day = list_to_integer(lists:sublist(DateTime_List, 5, 2)),
    DateTime_Hour = list_to_integer(lists:sublist(DateTime_List, 7, 2)),
    DateTime_Minute = list_to_integer(lists:sublist(DateTime_List, 9, 2)),
    DateTime_Second = 0,
    {{DateTime_Year, DateTime_Month, DateTime_Day}, {DateTime_Hour, DateTime_Minute, DateTime_Second}}.


%% 中央空调的设定温度及模式
get_temp_and_mode(Temp_and_mode) ->
    Temp_and_mode_tmp = lists:reverse(binary_to_list(<< <<X/integer>> || <<X>> <= <<Temp_and_mode:(2*8)>> >>)),
    Temp_and_mode_tmp_List = [?HELP:sub51(X) || X <- Temp_and_mode_tmp],
    lists:flatten(Temp_and_mode_tmp_List).

%% 中央空调风速档位
get_wind_speed_gears(Wind_speed_gears) ->
    Wind_speed_gears_tmp = binary_to_list(<< <<X/integer>> || <<X>> <= <<Wind_speed_gears:(1*8)>> >>),
    Wind_speed_gears_List = [?HELP:sub51(X) || X <- Wind_speed_gears_tmp],
    lists:flatten(Wind_speed_gears_List).

get_xxx_speed_used_time(XXX_speed_used_time) ->
    XXX_speed_used_time_tmp = lists:reverse(binary_to_list(<< <<X/integer>> || <<X>> <= <<XXX_speed_used_time:(4*8)>> >>)),
    %% 对每个数据进行减去51(33H)，645协议规定， sub51函数，返回的是"XX"(16进制)
    XXX_speed_used_time_List = [?HELP:sub51(X) || X <- XXX_speed_used_time_tmp],
    %% 进行扁平化为"XXXXXXXX"
    lists:flatten(XXX_speed_used_time_List).
%% 金额
get_amount(Amount) ->
    Amount_tmp = lists:reverse(binary_to_list(<< <<X/integer>> || <<X>> <= <<Amount:(4*8)>> >>)),
    %% 对每个数据进行减去51(33H)，645协议规定， sub51函数，返回的是"XX"(16进制)
    Amount_tmp_List = [?HELP:sub51(X) || X <- Amount_tmp],
    %% 进行扁平化为"XXXXXXXX"
    Amount_List = lists:flatten(Amount_tmp_List),
    %% 获取整数部分(3byte, 6个字符), 并拼接成字符串
    Amount_List1 = lists:sublist(Amount_List, 1, 6),
    %% 获取小数部分(1byte, 2个字符)
    Amount_List2 = lists:sublist(Amount_List, 7, 2),
    %% 拼接成完整数据 "整数部分.小数部分"
    lists:concat([Amount_List1, ".", Amount_List2]).

get_touch_task_id(TaskIdBinary) ->
    lists:flatten([?HELP:sub51(X) || <<X>> <= TaskIdBinary]).

get_3761_meter_id(MeterIdBinary) ->
    to_reverse_list(MeterIdBinary).

get_3761_date_time(DatetimeBinary) ->
    DateTime_List = to_reverse_list(DatetimeBinary),
    DateTime_Year = list_to_integer(lists:sublist(DateTime_List, 1, 2)) + 2000,
    DateTime_Month = list_to_integer(lists:sublist(DateTime_List, 3, 2)),
    DateTime_Day = list_to_integer(lists:sublist(DateTime_List, 5, 2)),
    DateTime_Hour = list_to_integer(lists:sublist(DateTime_List, 7, 2)),
    DateTime_Minute = list_to_integer(lists:sublist(DateTime_List, 9, 2)),
    DateTime_Second = 0,
    {{DateTime_Year, DateTime_Month, DateTime_Day}, {DateTime_Hour, DateTime_Minute, DateTime_Second}}.
    
get_3761_active_electricity_str(ActiveEletricityBinary) ->
    ActiveEletricityList = to_reverse_list(ActiveEletricityBinary),
    ActiveEletricityStr1 = lists:sublist(ActiveEletricityList, 1, 6),    
    ActiveEletricityStr2 = lists:sublist(ActiveEletricityList, 7, 4),
    lists:concat([ActiveEletricityStr1, ".", ActiveEletricityStr2]).

get_3761_active_power(<<Power2:16, Sign:1, Power1:7>> = _PowerBinary) ->    
    SignStr = get_sign_str(Sign), 
    Power2Str = to_reverse_list(<<Power2:16>>),
    Power1Str = to_reverse_list(<<Power1:8>>),
    lists:concat([SignStr, Power1Str, ".", Power2Str]).

get_3761_power_factor(<<Factor2:8, Sign:1, Factor1:7>> = _PowerFactorBin) ->
    SignStr = get_sign_str(Sign), 
    Factor2Tmp = to_reverse_list(<<Factor2:8>>),
    Factor1_2Str = lists:sublist(Factor2Tmp, 1, 1),
    Factor2Str = lists:sublist(Factor2Tmp, 2, 1),
    Factor1_1Str = to_reverse_list(<<Factor1:8>>),    
    lists:concat([SignStr, Factor1_1Str, Factor1_2Str, ".", Factor2Str]).

get_3761_voltage(VoltageBin) ->
    VoltageStrTmp = to_reverse_list(VoltageBin),
    VoltageStr1 = lists:sublist(VoltageStrTmp, 1, 3),
    VoltageStr2 = lists:sublist(VoltageStrTmp, 4, 1),
    lists:concat([VoltageStr1, ".", VoltageStr2]).

get_3761_current(<<Current2:16, Sign:1, Current1:7>> = _CurrentBin) ->
    SignStr = get_sign_str(Sign), 
    Current2Tmp = to_reverse_list(<<Current2:16>>),
    Current1_2Str = lists:sublist(Current2Tmp, 1, 1),
    Current2Str = lists:sublist(Current2Tmp, 2, 3),
    Current1_1Str = to_reverse_list(<<Current1:8>>),
    lists:concat([SignStr, Current1_1Str, Current1_2Str, ".", Current2Str]).

get_3761_apparent_power(<<ApparentPower2:16, Sign:1, ApparentPower1:7>> = _ApparentPowerBin) ->
    SignStr = get_sign_str(Sign), 
    ApparentPower2Str = to_reverse_list(<<ApparentPower2:16>>),
    ApparentPower1Str = to_reverse_list(<<ApparentPower1:8>>),
    lists:concat([SignStr, ApparentPower1Str, ".", ApparentPower2Str]).

get_sign_str(Sign) ->    
    case Sign of
        1 ->
            "-";
        0 ->
            ""
    end.

get_3761_active_electricity_float(ActiveEletricityBinary) ->
    list_to_float(get_3761_active_electricity_str(ActiveEletricityBinary)).

get_3761_accumulative_volume_str(AccumulativeVolumeBinary) ->
    AccumulativeVolumeList = to_reverse_list(AccumulativeVolumeBinary),
    AccumulativeVolumeStr1 = lists:sublist(AccumulativeVolumeList, 1, 6),
    AccumulativeVolumeStr2 = lists:sublist(AccumulativeVolumeList, 7, 2),
    lists:concat([AccumulativeVolumeStr1, ".", AccumulativeVolumeStr2]).

get_3761_accumulative_volume_float(AccumulativeVolumeBinary) ->
    list_to_float(get_3761_accumulative_volume_str(AccumulativeVolumeBinary)).

get_3761_calorie_str(CaloriesBinary) ->
    CaloriesList = to_reverse_list(CaloriesBinary),
    CaloriesStr1 = lists:sublist(CaloriesList, 1, 6),
    CaloriesStr2 = lists:sublist(CaloriesList, 7, 2),
    lists:concat([CaloriesStr1, ".", CaloriesStr2]).

get_3761_calorie_float(CaloriesBinary) ->
    list_to_float(get_3761_calorie_str(CaloriesBinary)).

get_pm1dot0_str(PM1dot0) ->
    PM1dot0_tmp = lists:reverse(binary_to_list(<< <<X/integer>> || <<X>> <= <<PM1dot0:(2*8)>> >>)),
    %% 对每个数据进行减去51(33H)，645协议规定， sub51函数，返回的是"XX"(16进制)
    PM1dot0_tmp_List = [?HELP:sub51(X) || X <- PM1dot0_tmp],
    %% 进行扁平化为"XXXXXXXX"
    PM1dot0_str = lists:flatten(PM1dot0_tmp_List),
    PM1dot0_str.

get_pm2dot5_str(PM2dot5) ->
    PM2dot5_tmp = lists:reverse(binary_to_list(<< <<X/integer>> || <<X>> <= <<PM2dot5:(2*8)>> >>)),
    %% 对每个数据进行减去51(33H)，645协议规定， sub51函数，返回的是"XX"(16进制)
    PM2dot5_tmp_List = [?HELP:sub51(X) || X <- PM2dot5_tmp],
    %% 进行扁平化为"XXXXXXXX"
    PM2dot5_str = lists:flatten(PM2dot5_tmp_List),
    PM2dot5_str.

get_pm10_str(PM10) ->    
    PM10_tmp = lists:reverse(binary_to_list(<< <<X/integer>> || <<X>> <= <<PM10:(2*8)>> >>)),
    %% 对每个数据进行减去51(33H)，645协议规定， sub51函数，返回的是"XX"(16进制)
    PM10_tmp_List = [?HELP:sub51(X) || X <- PM10_tmp],
    %% 进行扁平化为"XXXXXXXX"
    PM10_str = lists:flatten(PM10_tmp_List),
    PM10_str.

get_temperature_str(Temperature) ->
    Temperature_tmp = lists:reverse(binary_to_list(<< <<X/integer>> || <<X>> <= <<Temperature:(2*8)>> >>)),
    %% 对每个数据进行减去51(33H)，645协议规定， sub51函数，返回的是"XX"(16进制)
    Temperature_tmp_List = [?HELP:sub51(X) || X <- Temperature_tmp],
    %% 进行扁平化为"XXXXXXXX"
    Temperature_List = lists:flatten(Temperature_tmp_List),
    Sign = lists:sublist(Temperature_List, 1, 2),
    Temperature_str = 
        case Sign of
            "00" ->
                lists:sublist(Temperature_List, 3, 2);
            "01" ->
                "-" ++ lists:sublist(Temperature_List, 3, 2)
        end,
    Temperature_str.

get_humidity_str(Humidity) ->        
    Humidity_str = integer_to_list(Humidity - 51, 16),
    Humidity_str.

get_hcho_str(HCHO) ->
    HCHO_tmp = lists:reverse(binary_to_list(<< <<X/integer>> || <<X>> <= <<HCHO:(2*8)>> >>)),
    %% 对每个数据进行减去51(33H)，645协议规定， sub51函数，返回的是"XX"(16进制)
    HCHO_tmp_List = [?HELP:sub51(X) || X <- HCHO_tmp],
    %% 进行扁平化为"XXXXXXXX"
    HCHO_str = lists:flatten(HCHO_tmp_List),
    HCHO_str.

get_tvoc_str(TVOC) ->
    TVOC_tmp = lists:reverse(binary_to_list(<< <<X/integer>> || <<X>> <= <<TVOC:(2*8)>> >>)),
    %% 对每个数据进行减去51(33H)，645协议规定， sub51函数，返回的是"XX"(16进制)
    TVOC_tmp_List = [?HELP:sub51(X) || X <- TVOC_tmp],
    %% 进行扁平化为"XXXXXXXX"
    TVOC_str = lists:flatten(TVOC_tmp_List),
    TVOC_str.
    
get_co2_str(CO2) ->
    CO2_tmp = lists:reverse(binary_to_list(<< <<X/integer>> || <<X>> <= <<CO2:(2*8)>> >>)),
    %% 对每个数据进行减去51(33H)，645协议规定， sub51函数，返回的是"XX"(16进制)
    CO2_tmp_List = [?HELP:sub51(X) || X <- CO2_tmp],
    %% 进行扁平化为"XXXXXXXX"
    CO2_str = lists:flatten(CO2_tmp_List),
    CO2_str.

get_co_str(CO) ->
    CO_tmp = lists:reverse(binary_to_list(<< <<X/integer>> || <<X>> <= <<CO:(2*8)>> >>)),
    %% 对每个数据进行减去51(33H)，645协议规定， sub51函数，返回的是"XX"(16进制)
    CO_tmp_List = [?HELP:sub51(X) || X <- CO_tmp],
    %% 进行扁平化为"XXXXXXXX"
    CO_str = lists:flatten(CO_tmp_List),
    CO_str.
    
get_o2_str(O2) ->    
    O2_tmp = lists:reverse(binary_to_list(<< <<X/integer>> || <<X>> <= <<O2:(2*8)>> >>)),
    %% 对每个数据进行减去51(33H)，645协议规定， sub51函数，返回的是"XX"(16进制)
    O2_tmp_List = [?HELP:sub51(X) || X <- O2_tmp],
    %% 进行扁平化为"XXXXXXXX"
    O2_str = lists:flatten(O2_tmp_List),
    O2_str.
    

to_reverse_list(Binary) when is_binary(Binary) ->
    hex_util:to_hex(lists:reverse(binary_to_list(Binary)));
to_reverse_list(List) when is_binary(List) ->
    hex_util:to_hex(lists:reverse(List)).






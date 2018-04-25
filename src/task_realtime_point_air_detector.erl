-module (task_realtime_point_air_detector).

-include("analyze.hrl").
-include("analyze_config.hrl").
-include("print.hrl").

-export([start/2]).

start(Meter, Meter_blob) ->
    Datetime = analyze_meter_blob_util:get_report_time(Meter_blob),
    Datetime_str = ?HELP:getDateTimeStr(Datetime),
    PM1dot0_str = analyze_meter_blob_util:get_pm1dot0_str(Meter_blob),
    PM2dot5_str = analyze_meter_blob_util:get_pm2dot5_str(Meter_blob),
    PM10_str = analyze_meter_blob_util:get_pm10_str(Meter_blob),
    Temperature_str = analyze_meter_blob_util:get_temperature_str(Meter_blob),
    Humidity_str = analyze_meter_blob_util:get_humidity_str(Meter_blob),
    HCHO_str = analyze_meter_blob_util:get_hcho_str(Meter_blob),
    CO2_str = analyze_meter_blob_util:get_co2_str(Meter_blob),
    
    Data_point = string:join([Datetime_str, PM1dot0_str, PM2dot5_str, PM10_str, Temperature_str, Humidity_str,
                             HCHO_str, CO2_str], ?FS),
    case task_util:get_realtime_data_point_list(Meter) of
        {ok, Data_point_list} ->
            case task_util:save_realtime_data_point_list(Meter, [Data_point | Data_point_list]) of
                ok -> ok;
                {error, Reason1} ->
                    ?ERROR("~p save_realtime_data_point_list(~p) is error: ~p", [?MODULE, Meter, Reason1])   
            end;
        {error, Reason} ->
            ?ERROR("~p get_realtime_data_point_list(~p) is error: ~p", [?MODULE, Meter, Reason])
    end.




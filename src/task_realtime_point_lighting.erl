-module (task_realtime_point_lighting).

-include("analyze.hrl").
-include("analyze_config.hrl").
-include("print.hrl").

-export([start/2]).

start(Meter, Meter_blob) ->
    Datetime = analyze_meter_blob_util:get_report_time(Meter_blob),
    Datetime_str = ?HELP:getDateTimeStr(Datetime),
    Active_power_str = analyze_meter_blob_util:get_active_power_str(Meter_blob),
    Data_point = string:join([Datetime_str, Active_power_str], ?FS),
    case task_util:get_realtime_data_point_list(Meter) of
        {ok, Data_point_list} ->
            case task_util:save_realtime_data_point_list(Meter, [Data_point | Data_point_list]) of
                ok -> ok;
                {error, Reason1} ->
                    ?ERROR("save_realtime_data_point_list(~p) is error: ~p", [Meter, Reason1])   
            end;
        {error, Reason} ->
            ?ERROR("get_realtime_data_point_list(~p) is error: ~p", [Meter, Reason])
    end.

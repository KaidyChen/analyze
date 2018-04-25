-module (analyze_meter_ac_work_activities).

-include("analyze_meter.hrl").
-include("analyze.hrl").
-include("analyze_config.hrl").
-include("print.hrl").

-export([work_activities_init/3, update_work_activities/4, get_work_activities_str/1]).

get_work_activities_init() ->
    #ac_work_activities{
        sum_worktime = 0,
        sum_used_ele = 0.0,
        work_prev_temp = ?NONE_DATA,
        work_temp = #work_temp{work_avg_temp = ?NONE_DATA, sum_work_temp = 0.0, work_record_count = 0}
    }.

work_activities_init(Meter_type, Meter, Now) ->
    case analyze_meter_work_activities_store:lookup(Meter_type, Meter) of
        {ok, Work_activities = #ac_work_activities{prev_blob = Prev_blob}} when is_record(Prev_blob, blob) ->
            Datetime_prev = analyze_meter_blob_util:get_report_time(Prev_blob),
            case analyze_meter_util:is_a_new_date(Datetime_prev, Now) of
                false -> Work_activities;
                true -> 
                    save_work_activities(Meter_type, Meter, Work_activities, Datetime_prev),
                    get_work_activities_init()    
            end;
        {ok, Work_activities} ->
            get_work_activities_init();
        {error, _} ->
            get_work_activities_init()
    end.

update_work_activities(#ac_work_activities{prev_blob = Prev_blob} = Work_activities, Meter_blob, Meter_type, Meter) when is_record(Prev_blob, blob) ->
    Datetime_prev = analyze_meter_blob_util:get_report_time(Prev_blob),
    Datetime_cur = analyze_meter_blob_util:get_report_time(Meter_blob),
    case analyze_meter_util:is_a_new_date(Datetime_prev, Datetime_cur) of
        false -> get_new_work_activities(Work_activities, Meter_blob);
        true -> 
            save_work_activities(Meter_type, Meter, Work_activities, Datetime_prev),
            New_work_activities = get_work_activities_init(),
            New_work_activities#ac_work_activities{prev_blob = Meter_blob}
    end;
update_work_activities(Work_activities, Meter_blob, Meter_type, Meter) ->
    Work_activities#ac_work_activities{prev_blob = Meter_blob}.

get_new_work_activities(Work_activities, Meter_blob) ->
    #ac_work_activities{
        prev_blob = Prev_blob,
        sum_worktime = Sum_worktime,
        sum_used_ele = Sum_used_ele,
        work_prev_temp = Work_prev_temp,
        work_temp = Work_temp
    } = Work_activities,
    Worktime = cal_worktime(Prev_blob, Meter_blob),
    Used_ele = cal_used_ele(Prev_blob, Meter_blob),
    New_prev_blob = Meter_blob,
    New_sum_worktime = Sum_worktime + Worktime,
    New_sum_used_ele = Sum_used_ele + Used_ele,
    Active_power_cur = analyze_meter_blob_util:get_active_power_float(Meter_blob),
    Temp_str = analyze_meter_blob_util:get_temp_str(Meter_blob),
    {New_work_prev_temp, New_work_temp} = try list_to_float(Temp_str) of
        Temp ->
            case Active_power_cur >= ?WORKING_ACTIVE_POWER of
                true ->
                    #work_temp{   
                        sum_work_temp = Sum_work_temp,
                        work_record_count =  Work_record_count
                    } = Work_temp,
                    New_work_temp_tmp = #work_temp{
                        work_avg_temp = (Sum_work_temp + Temp) / (Work_record_count+1),
                        sum_work_temp = Sum_work_temp + Temp,
                        work_record_count =  Work_record_count + 1
                    },
                    {Work_prev_temp, New_work_temp_tmp};
                false ->
                    {Temp, Work_temp}
            end
    catch
        _:_ ->
            {Work_prev_temp, Work_temp}
    end,
    #ac_work_activities{
        prev_blob = New_prev_blob,
        sum_worktime = New_sum_worktime,
        sum_used_ele = New_sum_used_ele,
        work_prev_temp = New_work_prev_temp,
        work_temp = New_work_temp
    }.

cal_worktime(Prev_blob, Meter_blob) ->
    Datetime_prev = analyze_meter_blob_util:get_report_time(Prev_blob),
    Datetime_cur = analyze_meter_blob_util:get_report_time(Meter_blob),
    Active_power_prev = analyze_meter_blob_util:get_active_power_float(Prev_blob),
    Active_power_cur = analyze_meter_blob_util:get_active_power_float(Meter_blob),
    Electric_power_diff = cal_used_ele(Prev_blob, Meter_blob),
    Second_diff = ?HELP:getTimeDiff(Datetime_prev, Datetime_cur),
    Worktime = analyze_meter_util:get_used_time(Active_power_prev, Active_power_cur, Second_diff, Electric_power_diff),
    Worktime.
    % case {Active_power_prev >= ?WORKING_ACTIVE_POWER, Active_power_cur >= ?WORKING_ACTIVE_POWER} of
    %     {true, true} ->
    %         Second_diff;
    %     {true, false} ->
    %         %% 0.5h = 1800s
    %         case Second_diff =< 1800 of
    %             true -> round(Electric_power_diff / (Active_power_prev / 1000) * ?ONE_HOUR_SECONDS);
    %             false -> 0  %% 忽略两点之间的时长
    %         end;
    %     {false, true} ->
    %         case Second_diff =< 1800 of
    %             true -> Second_diff;
    %             false -> round(Electric_power_diff / (Active_power_cur / 1000) * ?ONE_HOUR_SECONDS)
    %         end;
    %     {false, false} ->
    %         0
    % end.

cal_used_ele(Prev_blob, Meter_blob) ->
    Electric_power_prev = analyze_meter_blob_util:get_electric_power_float(Prev_blob),
    Electric_power_cur = analyze_meter_blob_util:get_electric_power_float(Meter_blob),
    Electric_power_cur - Electric_power_prev.



get_work_activities_str(Work_activities) ->
    #ac_work_activities{
        sum_worktime = Sum_worktime,
        sum_used_ele = Sum_used_ele,
        work_prev_temp = Work_prev_temp,
        work_temp = Work_temp
    } = Work_activities,
    Sum_worktime_str = get_sum_worktime_str(Sum_worktime),
    Sum_used_ele_str = get_sum_used_ele_str(Sum_used_ele),
    Work_prev_temp_str = get_temp_str(Work_prev_temp),
    Work_avg_temp_str = get_temp_str(Work_temp#work_temp.work_avg_temp),
    string:join([Sum_worktime_str, Sum_used_ele_str, Work_prev_temp_str, Work_avg_temp_str], "#").

get_sum_worktime_str(Sum_worktime) ->
    How_hour = Sum_worktime div ?ONE_HOUR_SECONDS,
    Hou_minute = (Sum_worktime rem ?ONE_HOUR_SECONDS) div 60,
    lists:concat([integer_to_list(How_hour), "h", Hou_minute, "min"]).

get_sum_used_ele_str(Sum_used_ele) ->
    ?HELP:float_to_decimal_str(Sum_used_ele, 2).

get_temp_str(Temp) when is_list(Temp) ->
    Temp;
get_temp_str(Temp) when is_float(Temp) ->
    ?HELP:float_to_decimal_str(Temp, 2).

save_work_activities(Meter_type, Meter, Work_activities, Datetime) ->
    {Date, _time} = Datetime,
    {Year, Month, _Day} = Date,
    Filepath = analyze_meter_work_activities_util:get_work_activities_filepath(Year, Month, Meter),
    Date_str = ?HELP:dateToStr(Date), 
    Work_activities_str = get_work_activities_str(Work_activities),  
    Content = string:join([Date_str, Work_activities_str], ?FS),
    case save_work_activities_(Filepath, Content) of
        ok              -> ok;
        {error, Reason} -> ?ERROR("save_work_activities Meter:~p is error:~p", [ Meter, Reason])
    end.

save_work_activities_(Filepath, Append_content) ->
    filelib:ensure_dir(Filepath),
    ?HELP:append_content(Filepath, Append_content).
    

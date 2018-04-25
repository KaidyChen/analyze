-module (analyze_meter_central_ac_work_activities).

-include("analyze_meter.hrl").
-include("analyze.hrl").
-include("analyze_config.hrl").
-include("print.hrl").

-export([work_activities_init/3, update_work_activities/4, get_work_activities_str/1]).

get_work_activities_init() ->
    #central_ac_work_activities{
        sum_worktime = 0,
        work_prev_temp = ?NONE_DATA,
        work_temp = #work_temp{work_avg_temp = ?NONE_DATA, sum_work_temp = 0.0, work_record_count = 0}
    }.

work_activities_init(Meter_type, Meter, Now) ->
    case analyze_meter_work_activities_store:lookup(Meter_type, Meter) of
        {ok, Work_activities = #central_ac_work_activities{prev_blob = Prev_blob}} when is_record(Prev_blob, blob) ->
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

update_work_activities(#central_ac_work_activities{prev_blob = Prev_blob} = Work_activities, Meter_blob, Meter_type, Meter) when is_record(Prev_blob, blob) ->
    Datetime_prev = analyze_meter_blob_util:get_report_time(Prev_blob),
    Datetime_cur = analyze_meter_blob_util:get_report_time(Meter_blob),
    case analyze_meter_util:is_a_new_date(Datetime_prev, Datetime_cur) of
        false -> get_new_work_activities(Work_activities, Meter_blob);
        true -> 
            save_work_activities(Meter_type, Meter, Work_activities, Datetime_prev),
            New_work_activities = get_work_activities_init(),
            New_work_activities#central_ac_work_activities{prev_blob = Meter_blob}
            
    end;
update_work_activities(Work_activities, Meter_blob, Meter_type, Meter) ->
    Work_activities#central_ac_work_activities{prev_blob = Meter_blob}.

get_new_work_activities(Work_activities, Meter_blob) ->
    #central_ac_work_activities{
        prev_blob = Prev_blob,
        sum_worktime = Sum_worktime,
        work_prev_temp = Work_prev_temp,
        work_temp = Work_temp
    } = Work_activities,
    Worktime = cal_worktime(Prev_blob, Meter_blob),
    New_prev_blob = Meter_blob,
    New_sum_worktime = Sum_worktime + Worktime,
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
    #central_ac_work_activities{
        prev_blob = New_prev_blob,
        sum_worktime = New_sum_worktime,
        work_prev_temp = New_work_prev_temp,
        work_temp = New_work_temp
    }.

cal_worktime(Prev_blob, Meter_blob) ->
    Used_time_sum_prev = analyze_meter_blob_util:get_low_medium_high_speed_used_time_sum(Prev_blob),
    Used_time_sum_cur = analyze_meter_blob_util:get_low_medium_high_speed_used_time_sum(Meter_blob),
    Used_time_sum_cur - Used_time_sum_prev.

get_work_activities_str(Work_activities) ->
    #central_ac_work_activities{
        sum_worktime = Sum_worktime,
        work_prev_temp = Work_prev_temp,
        work_temp = Work_temp
    } = Work_activities,
    Sum_worktime_str = get_sum_worktime_str(Sum_worktime),
    Work_prev_temp_str = get_temp_str(Work_prev_temp),
    Work_avg_temp_str = get_temp_str(Work_temp#work_temp.work_avg_temp),
    string:join([Sum_worktime_str, Work_prev_temp_str, Work_avg_temp_str], "#").

%% 中央空调时长是分钟统计
get_sum_worktime_str(Sum_worktime) ->
    How_hour = Sum_worktime div 60,
    Hou_minute = Sum_worktime rem 60,
    lists:concat([integer_to_list(How_hour), "h", Hou_minute, "min"]).

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
        {error, Reason} -> ?ERROR("save_work_activities Meter:~p is error:~p", [Meter, Reason])
    end.

save_work_activities_(Filepath, Append_content) ->
    filelib:ensure_dir(Filepath),
    ?HELP:append_content(Filepath, Append_content).

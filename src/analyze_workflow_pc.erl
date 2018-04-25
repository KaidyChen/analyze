-module(analyze_workflow_pc).

-include("print.hrl").
-include("analyze.hrl").
-include("analyze_config.hrl").
-include("analyze_meter.hrl").

-export([init/2, update_workflow/4, get_realtime_workflow_str/1]).

-define(WORK_ACTIVE_POWER, 5).
-define(FACTOR, 0.2).
-define(FACTOR_2, 0.8).

-define(INIT, 0).           % 初始化状态
-define(POWER_OFF, 1).      % 关机状态
-define(STANDBY, 2).        % 待机状态
-define(USUAL, 3).          % 活跃程度一般状态
-define(ACTIVE, 4).         % 活跃状态
-define(STARTING_UP, 5).    % 开机状态

-define(STATUS_STRING_LIST, [
       {?INIT, "null"},
       {?POWER_OFF, "power off"},
       {?STANDBY, "standby"},
       {?USUAL, "usual"},
       {?ACTIVE, "active"},
       {?STARTING_UP, "starting up"}
]).

get_init() ->
    #workflow{
        status = ?INIT, 
        platform_list = [], 
        mean_max = 0.0, 
        sum_work_status = 0.0, 
        count = 0,
        starting_up_time = undefined,
        power_off_time = undefined,
        sum_worktime = 0
    }.

init(Meter_type, Meter) ->
    case analyze_realtime_workflow:lookup(Meter_type, Meter) of
        {ok, Readtime_workflow} ->
            Readtime_workflow;
        {error, _} ->
            get_init()
    end.
    
new_date_init_workflow(Workflow, Meter_blob) ->
    Workflow#workflow{
        prev_blob = Meter_blob,
        platform_list = [], 
        mean_max = 0.0, 
        sum_work_status = 0.0, 
        count = 0,
        starting_up_time = undefined,
        sum_worktime = 0
    }.

get_realtime_workflow_str(Workflow) ->
    #workflow{
        status = Status, 
        starting_up_time = Starting_up_time,
        power_off_time = Power_off_time,
        sum_worktime = Sum_worktime
    } = Workflow,
    Status_str = get_status_str(Status),
    Starting_up_time_str = get_time_str(Starting_up_time),
    Sum_worktime_str = get_sum_worktime_str(Sum_worktime),
    Power_off_time_str = get_time_str(Power_off_time),
    string:join([Status_str, Starting_up_time_str, Sum_worktime_str, Power_off_time_str], "#").

update_workflow(Meter_type, Meter, Workflow = #workflow{prev_blob = Prev_blob}, Meter_blob) when is_record(Prev_blob, blob) ->
    Datetime_prev = analyze_meter_blob_util:get_report_time(Prev_blob),
    Datetime_cur = analyze_meter_blob_util:get_report_time(Meter_blob),
    case analyze_meter_util:is_a_new_date(Datetime_prev, Datetime_cur) of
        true -> new_date_init_workflow(Workflow, Meter_blob);
        false -> update_workflow_(Meter_type, Meter, Workflow, Meter_blob)
    end;
update_workflow(Meter_type, Meter, Workflow = #workflow{prev_blob = Prev_blob}, Meter_blob) ->
    Workflow#workflow{prev_blob = Meter_blob}.

update_workflow_(Meter_type, Meter, Workflow, Meter_blob) ->
    Active_power = analyze_meter_blob_util:get_active_power_float(Meter_blob),
    Report_time = analyze_meter_blob_util:get_report_time(Meter_blob),
    #workflow{
        prev_blob = Prev_blob,
        platform_list = Platform_list, 
        mean_max = Mean_max, 
        sum_work_status = Sum_work_status, 
        count = Count, 
        sum_worktime = Sum_worktime,
        status = Prev_status
    } = Workflow,
    {New_sum_works_status, New_count} = case (Active_power > ?WORK_ACTIVE_POWER) of
        true  -> {Sum_work_status + Active_power, Count + 1};
        false -> {Sum_work_status, Count} 
    end,
    Mean_2 = case (New_count > 0) of 
        true  -> New_sum_works_status / New_count;
        false -> 0.0
    end,
    {New_platform_list, New_mean_max, New_status} =  case get_prev_active_power(Platform_list) of
        {ok, Prev_active_power} -> 
            case (Prev_status =:= 5) orelse is_new_platform(Active_power, Prev_active_power) of
                true  ->
                    New_platform_list_tmp = add_to_new_platform(Active_power, Platform_list),
                    [Platform | _] = Platform_list,
                    Mean = get_mean(Platform),
                    New_mean_max_tmp = get_mean_max(Mean_max, Mean),
                    New_status_tmp = get_new_status(Active_power, Mean_2, Mean_max),
                    {New_platform_list_tmp, New_mean_max_tmp, New_status_tmp};
                false ->
                    New_platform_list_tmp = add_to_old_platform(Active_power, Platform_list),
                    {New_platform_list_tmp, Mean_max, Prev_status}
            end;
        {error, not_found} ->
            New_platform_list_tmp = add_to_new_platform(Active_power, Platform_list),
            New_status_tmp = get_new_status(Active_power, Active_power, Active_power),
            {New_platform_list_tmp, Mean_max, New_status_tmp}
    end,
    {Is_status_change, Status_tmp} = case (New_status =/= Prev_status) of
        false  -> {false, New_status};
        true -> 
            Status = case (Prev_status =:= 1) of
                true -> 
                    5;
                false ->
                    New_status     
            end,
            Status_str = get_status_str(Status),
            save_status(Meter, Report_time, Status_str),
            {true, Status}     
    end,

    New_sum_worktime = Sum_worktime + cal_sum_worktime(Workflow, Prev_blob, Meter_blob),

    New_workflow = Workflow#workflow{
        prev_blob = Meter_blob,
        platform_list = New_platform_list, 
        mean_max = New_mean_max, 
        sum_work_status = New_sum_works_status,
        count = New_count,
        sum_worktime = New_sum_worktime,
        status = Status_tmp
    },

    case {Is_status_change, Status_tmp} of
        {true, 1} -> 
            New_workflow#workflow{power_off_time = Report_time};
        {true, 5} ->
            case New_workflow#workflow.starting_up_time of
                undefined ->
                    New_workflow#workflow{starting_up_time = Report_time};
                _ ->
                    New_workflow
            end;
        _ -> New_workflow
    end.

get_prev_active_power([Platform | _]) ->
    Active_power_list = Platform,
    [Prev_active_power | _] = Active_power_list,
    {ok, Prev_active_power};
get_prev_active_power([]) ->
    {error, not_found}.

is_new_platform(Active_power, Prev_active_power) ->
    {Max, Min} = case (Active_power > Prev_active_power) of
        true  -> {Active_power, Prev_active_power};
        false -> {Prev_active_power, Active_power}     
    end,
    (Max-Min) > (Max*?FACTOR).

add_to_old_platform(Active_power, [Platform | Tail]) ->
    [[Active_power | Platform] | Tail].

add_to_new_platform(Active_power, Platform_list) ->
    [[Active_power] | Platform_list].

get_mean(Platform) ->
    Active_power_list = Platform,
    Length = length(Active_power_list), 
    New_mean = case (Length > 0) of
        true  -> lists:sum(Active_power_list) / Length;
        false -> 0.0    
    end.
    

get_mean_max(Mean_max, New_mean) ->
    case (Mean_max > New_mean) of
        true  -> Mean_max;
        false -> New_mean
    end.

get_new_status(Mean, Mean_2, Mean_max) ->
    Standby = Mean_2 * ?FACTOR_2,
    Active = Mean_max * ?FACTOR_2,
    if
        Mean =< ?WORK_ACTIVE_POWER ->
            1;
        (?WORK_ACTIVE_POWER < Mean) andalso (Mean < Standby) ->
            2;
        (Standby =< Mean) andalso (Mean < Active) ->
            3;
        Mean >= Active ->
            4;
        true ->
            5
    end.   

save_status(Meter, Datetime, Status_str) ->
    {Date, _time} = Datetime,
    {Year, Month, _Day} = Date,
    Filepath = analyze_workflow_util:get_workflow_filepath(Year, Month, Meter),
    Datetime_str = ?HELP:getDateTimeStr(Datetime),        
    Content = string:join([Datetime_str, Status_str], ?FS),
    filelib:ensure_dir(Filepath),
    case save_workflow_status(Filepath, Content) of
        ok              -> ?PRINT("~s ~s~n", [Datetime_str, Content]), ok;
        {error, Reason} -> ?ERROR("save_workflow_status Meter:~p is error:~p", [Meter, file:format_error(Reason)])
    end.
                                                                                                                    
save_workflow_status(Filepath, Append_content) ->
    ?HELP:append_content(Filepath, Append_content).

cal_sum_worktime(Workflow, Prev_blob, Meter_blob) ->
    Datetime_prev = analyze_meter_blob_util:get_report_time(Prev_blob),
    Datetime_cur = analyze_meter_blob_util:get_report_time(Meter_blob),
    Active_power_prev = analyze_meter_blob_util:get_active_power_float(Prev_blob),
    Active_power_cur = analyze_meter_blob_util:get_active_power_float(Meter_blob),
    Electric_power_diff = cal_used_ele(Prev_blob, Meter_blob),
    Second_diff = ?HELP:getTimeDiff(Datetime_prev, Datetime_cur),
    Used_time = analyze_meter_util:get_used_time(Active_power_prev, Active_power_cur, Second_diff, Electric_power_diff),
    Used_time.

get_status_str(Status) ->
     proplists:get_value(Status, ?STATUS_STRING_LIST).

get_time_str(Datetime) ->
    case (Datetime =:= undefined) of
        true -> ?NONE_DATA;
        false -> ?HELP:getDateTimeStr(Datetime)       
    end.

get_sum_worktime_str(Sum_worktime) ->
    How_hour = Sum_worktime div ?ONE_HOUR_SECONDS,
    Hou_minute = (Sum_worktime rem ?ONE_HOUR_SECONDS) div 60,
    lists:concat([integer_to_list(How_hour), "h", Hou_minute, "min"]).

cal_used_ele(Prev_blob, Meter_blob) ->
    Electric_power_prev = analyze_meter_blob_util:get_electric_power_float(Prev_blob),
    Electric_power_cur = analyze_meter_blob_util:get_electric_power_float(Meter_blob),
    Electric_power_cur - Electric_power_prev.


                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            

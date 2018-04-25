-module (analyze_meter_work_activities_util).

-compile([export_all]).

-include("analyze.hrl").
-include("analyze_config.hrl").

get_work_activities_filepath(Year, Month, Meter) ->    
    Filepath = ?HELP:get_year_month_file_path(Year, Month, Meter, ?WORK_ACTIVITIES),
    Filepath.

get_work_activities_str(?AC_TYPE, Work_activities) ->
    analyze_meter_ac_work_activities:get_work_activities_str(Work_activities);
get_work_activities_str(?CENTRAL_AC_TYPE, Work_activities) ->
    analyze_meter_central_ac_work_activities:get_work_activities_str(Work_activities);
get_work_activities_str(_, _) ->
    ?NULL.
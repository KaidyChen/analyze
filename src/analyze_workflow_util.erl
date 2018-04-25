-module (analyze_workflow_util).

-compile([export_all]).

-include("analyze.hrl").
-include("analyze_config.hrl").

get_workflow_filepath(Year, Month, Meter) ->    
    Filepath = ?HELP:get_year_month_file_path(Year, Month, Meter, ?WORKFLOW),
    Filepath. 
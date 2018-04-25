-module(analyze_trigger_task).

-include("print.hrl").

-export([run_tasks/4]).

run_tasks(Meter_type, Meter, Msg_type, Meter_blob) ->
    case analyze_meter_util:lookup_tasks(Meter_type, Msg_type) of
        {ok, Tasks_List} ->
            ?PRINT("~p~n", [Tasks_List]),
            Func = fun
                ({M, F, A})  ->
                    NewA = case A of
                        [] ->
                            [Meter, Meter_blob];
                        _Other ->
                            _Other
                    end,
                    run({M, F, NewA})
            end,
            [Func(MFA) || MFA <- Tasks_List];
        {error, not_found} ->
            ?PRINT("not found tasks of {~p, ~p}~n", [Meter_type, Msg_type]),
            ok
    end.

%% run the task
run({M, F, A} = Task) ->
    try apply(M, F, A) of
        _ -> ok 
    catch
        Class:Reason ->
            ?ERROR("analyze_trigger_task ~p error: ~p~n", [Task, Reason])
    end.
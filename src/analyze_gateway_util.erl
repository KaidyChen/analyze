-module (analyze_gateway_util).

-compile([export_all]).

get_gateway_pid_by_gateway(Gateway) ->
    case analyze_gateway_pid:lookup(Gateway) of
        {ok, Pid} ->
            case erlang:is_process_alive(Pid) of
                true ->
                    {ok, Pid};
                false ->
                    {error, not_alive}
            end;
        {error, Reason} ->
            {error, Reason}
    end.
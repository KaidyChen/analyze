-module(analyze_meter_quota).

-include("analyze.hrl").
-include("analyze_config.hrl").
-include("print.hrl").
-include("meter_quota.hrl").

-export([start/4, new_month_update_quota/4]).

new_month_update_quota(Meter_type, Meter, Gateway, Cur_quantity) ->
    case analyze_meter_quota_server:get_quota(Meter_type, Meter) of
        {error, _} -> ok;
        {ok, Meter_quota} ->
            New_meter_quota = Meter_quota#meter_quota{
                base_quantity = Cur_quantity,
                cur_quantity = Cur_quantity,
                warn_time_1 = 0,
                warn_time_2 = 0
            },
            analyze_meter_quota_server:update_quota(New_meter_quota)
    end.

start(Meter_type, Meter, Gateway, Cur_quantity) ->
    case analyze_meter_quota_server:get_quota(Meter_type, Meter) of
        {error, _} -> ok;
        {ok, Meter_quota} ->
            #meter_quota{
                key = {Meter_type, Meter},
                base_quantity = Base_quantity,   
                quota = Quota,          
                mode = Mode,   
                warn_time_1 = Warn_time_1,  
                warn_time_2 = Warn_time_2    
            } = Meter_quota,
            Used_quantity = Cur_quantity - Base_quantity,
            Delta = Used_quantity - Quota, 
            {New_warn_time_1, New_warn_time_2} = case {(-(Quota/30) =< Delta) andalso (Delta < 0.0), Delta >= 0.0} of
                {true, _} ->
                    case Warn_time_1 of
                        0 ->
                            ?ERROR("The ~p/~p used of :~p is close to limit:~p.~n", [Meter_type, Meter, Used_quantity, Quota]),
                            %% 推送报警
                            push_msg(Meter_type, Meter, Used_quantity, Quota),
                            {1, Warn_time_2};
                        1 -> 
                            {Warn_time_1, Warn_time_2}
                    end;
                {_, true} ->
                    case Warn_time_2  of
                        0 ->
                            ?ERROR("The ~p/~p used of :~p is attained to limit:~p.~n", [Meter_type, Meter, Used_quantity, Quota]),
                            %% 推送报警
                            push_msg(Meter_type, Meter, Used_quantity, Quota),
                            {Warn_time_1, 1};
                        1 ->
                            ?ERROR("The ~p/~p used of :~p is attained to limit:~p.~n", [Meter_type, Meter, Used_quantity, Quota]),
                            %% 推送报警
                            push_msg(Meter_type, Meter, Used_quantity, Quota),
                            case Mode of
                                0 -> ok; % 只报警不跳闸
                                1 ->     % 报警后跳闸
                                    Operation_type = ?TZ,
                                    Operation_argv = "",
                                    analyze_meter_util:send_to_gateway_exec(Gateway, Meter_task = {Meter, Meter_type, Operation_type, Operation_argv})
                            end,
                            {Warn_time_1, 2};
                        _ -> 
                            {Warn_time_1, Warn_time_2}
                    end;
                {_, _} ->
                    {Warn_time_1, Warn_time_2}
            end,
            New_meter_quota = Meter_quota#meter_quota{
                cur_quantity = Cur_quantity,
                warn_time_1 = New_warn_time_1,
                warn_time_2 = New_warn_time_2
            },
            analyze_meter_quota_server:update_quota(New_meter_quota)
    end.



%% 推送消息
push_msg(Meter_type, Meter, Used_quantity, Quota) ->
    Msg_type = "Quota_info",
    Msg_body = get_msg_body(Meter_type, Meter, Used_quantity, Quota),
    analyze_push_service:push(Msg_type, Msg_body),
    ok.

get_msg_body(Meter_type, Meter, Used_quantity, Quota) ->
    Residues_quantity_str = ?HELP:float_to_decimal_str(Quota - Used_quantity, 2),
    Quota_str = ?HELP:float_to_decimal_str(Quota, 2),
    string:join([Meter_type, Meter, Residues_quantity_str, Quota_str], "#").


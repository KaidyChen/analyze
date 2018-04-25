-module(analyze_meter_recharge).

-include("analyze.hrl").
-include("analyze_config.hrl").
-include("print.hrl").
-include("cmd_obj.hrl").

-export([get_last_record_of_recharge/2]).
-export([is_exist_unknown_of_recharge/3, is_exist_unknown_of_recharge/2]).
-export([add_recharge_record/3]).
-export([start/4]).

-define(SUCCESS, "Success").
-define(UNKNOWN, "Unknown").
-define(FAILURE, "Failure").


get_last_record_of_recharge(Meter_type, Meter) ->
    case get_recharge_list(Meter) of
        {ok, Recharge_list} ->
            [Last_record_of_recharge | _] = lists:reverse(Recharge_list),
            {ok, Last_record_of_recharge};
        {error, Reason} ->
            {error, Reason}
    end.

is_exist_unknown_of_recharge(Meter_type, Meter) ->
    case get_last_record_of_recharge(Meter_type, Meter) of
        {ok, Last_record_of_recharge} ->
            is_exist_unknown_of_recharge(Meter_type, Meter, Last_record_of_recharge);
        {error, enoent} ->
            false;
        {error, Reason} -> 
            true
    end.

is_exist_unknown_of_recharge(Meter_type, Meter, Last_record_of_recharge) ->
    case string:tokens(Last_record_of_recharge, ?FS) of
        [_, _, _, _, _, ?UNKNOWN] -> 
            true;
        [_, _, _, _, _, Recharge_status] when (Recharge_status =:= ?SUCCESS) orelse (Recharge_status =:= ?FAILURE) -> 
            false;
        _ ->
            ?ERROR("Last_record_of_recharge:~p of Meter: ~p is not match~n", [Last_record_of_recharge, Meter]),
            false
    end.

add_recharge_record(Meter_type, Meter, {Order_no, Recharge_num, Recharge_date, Recharge_time, Recharge_money, Recharge_status}) ->
    RechargeRecord = string:join([Order_no, Recharge_num, Recharge_date, Recharge_time, Recharge_money, Recharge_status], ?FS),
    case append_recharge_record(Meter, RechargeRecord) of
        ok              -> ?DEBUG("~p add_recharge_record is ok~n", [Meter]), ok;
        {error, Reason} -> ?ERROR("Meter: ~p append_rechargeRecord is error:~p", [Meter, Reason])
    end.

start(Meter_type, Meter, Cq_weight, Last_record_of_recharge) ->
    case string:tokens(Last_record_of_recharge, ?FS) of
        [Order_no, Recharge_num, Recharge_date, Recharge_time, Recharge_money, ?UNKNOWN] ->
            Recharge_num_int =  list_to_integer(Recharge_num),
            case read_recharge_num(Meter_type, Meter, Cq_weight) of
                %% 读回的购电次数与记录中的购电次数一样, 则更新记录中状态为?SUCCESS
                {ok, Recharge_num_int} -> 
                    %% 还要推送充值已成功消息
                    recharge_result(Meter_type, Meter, {Order_no, Recharge_num, Recharge_date, Recharge_time, Recharge_money, ?SUCCESS});
                {ok, Meter_recharge_num} when ((Meter_recharge_num + 1) =:= Recharge_num_int) -> 
                    case send_recharge(Meter_type, Meter, Cq_weight, Recharge_money, Recharge_num_int) of
                        {ok, Recharge_num_int} ->
                            %% 还要推送充值已成功消息
                            {Date, Time} = ?HELP:datetime_now(),
                            New_recharge_date = ?HELP:dateToStr(Date),
                            New_Recharge_time = ?HELP:timeToStr(Time),
                            recharge_result(Meter_type, Meter, {Order_no, Recharge_num, New_recharge_date, New_Recharge_time, Recharge_money, ?SUCCESS});
                        {false, read_recharge_num_failure} ->
                            %% 下发充值后，未返回成功，但是读取购电次数也失败，则不修改充值记录文件
                            ok;
                        false ->
                            ok
                    end;
                false -> ?ERROR("Meter: ~p can't read_recharge_num", [Meter])
            end;
        _ -> ?ERROR("Meter:~p last_record_format_is_error: ~p", [Last_record_of_recharge])
    end,

    ok.

%% Internal functions

recharge_result(Meter_type, Meter, {Order_no, Recharge_num, Recharge_date, Recharge_time, Recharge_money, Recharge_status}) ->

    %% 推送
    Msg_type = "Recharge_result",
    Msg_body = get_msg_body(Meter_type, Meter, {Order_no, Recharge_num, Recharge_date, Recharge_time, Recharge_money, Recharge_status}),
    analyze_push_service:push(Msg_type, Msg_body),

    %% 记录入文件
    add_recharge_record(Meter_type, Meter, {Order_no, Recharge_num, Recharge_date, Recharge_time, Recharge_money, Recharge_status}),
    ok.

get_msg_body(Meter_type, Meter, {Order_no, Recharge_num, Recharge_date, Recharge_time, Recharge_money, Recharge_status}) ->
    string:join([Meter_type, Meter, Order_no, Recharge_num, Recharge_date, Recharge_time, Recharge_money, Recharge_status], "#").


get_recharge_list(Meter) ->
    Recharge_filepath = get_filepath_of_recharge(Meter),
    case file:read_file(Recharge_filepath) of
        {ok, Data_Bin}    -> 
            Data_List = binary_to_list(Data_Bin),
            DataLine_List = string:tokens(Data_List, ?NL),
            {ok, DataLine_List};
        {error, Reason} -> {error, Reason}
    end.

get_filepath_of_recharge(Meter) ->
    Recharge_filename = lists:concat([Meter, ?SUFFIX]),
    filename:join([?RECHARGEDIR, Recharge_filename]).

read_recharge_num(Meter_type, Meter, Cq_weight) ->
    read_recharge_num_(Meter_type, Meter, Cq_weight, 1).

read_recharge_num_(Meter_type, Meter, Cq_weight, Read_count) when (Read_count > 0)->
    Cmd_obj = #cmd_obj{
        eqpt_type = Meter_type, 
        eqpt_id_code = Meter, 
        cmd_type = "dsj", 
        cmd_id = "gdcs", 
        cmd_data = ""
    },
    
    case send_cmd_to_middleware:send(Cmd_obj, Cq_weight) of
        {ok, Return_data} ->
            ?DEBUG("Meter:~p ~s~n", [Meter, Return_data]),
            case send_cmd_to_middleware:get_result(Return_data) of
                {ok, Meter_recharge_num} ->
                    {ok, list_to_integer(Meter_recharge_num)};
                false ->
                    read_recharge_num_(Meter_type, Meter, Cq_weight, Read_count-1)
            end;
        _ ->
            read_recharge_num_(Meter_type, Meter, Cq_weight, Read_count-1)
    end;
read_recharge_num_(_Meter_type, _Meter, _Cq_weight, 0) ->
    false.

send_recharge(Meter_type, Meter, Cq_weight, Recharge_money, Recharge_num) ->
    send_recharge_(Meter_type, Meter, Cq_weight, Recharge_money, Recharge_num, 1).

send_recharge_(Meter_type, Meter, Cq_weight, Recharge_money, Recharge_num_int, Recharge_count) when (Recharge_count > 0) ->
    Cmd_obj = #cmd_obj{
        eqpt_type = Meter_type, 
        eqpt_id_code = Meter, 
        cmd_type = "xsj", 
        cmd_id = "cz", 
        cmd_data = Recharge_money
    },

    case send_cmd_to_middleware:send(Cmd_obj, Cq_weight) of
        {ok, Return_data} ->
            ?DEBUG("Meter:~p ~s~n", [Meter, Return_data]),
            case send_cmd_to_middleware:get_result(Return_data) of
                {ok, Meter_recharge_money_and_num} ->
                    case string:tokens(Meter_recharge_money_and_num, ",") of
                        [Meter_recharge_money, Meter_recharge_num] ->
                            {ok, list_to_integer(Meter_recharge_num)};
                        _ ->
                            ?ERROR("Meter: ~p Meter_recharge_money_and_num:~s format is error", [Meter, Meter_recharge_money_and_num]),
                            false
                    end;
                false ->
                    case read_recharge_num(Meter_type, Meter, Cq_weight) of
                        %% 读回的购电次数与记录中的购电次数一样
                        {ok, Recharge_num_int} -> 
                            {ok, Recharge_num_int};
                        {ok, Meter_recharge_num} when ((Meter_recharge_num + 1) =:= Recharge_num_int) -> 
                            send_recharge_(Meter_type, Meter, Cq_weight, Recharge_money, Recharge_num_int, Recharge_count-1);
                        false -> 
                            %% 若下发充值返回错误后，读取购电次数也失败，
                            ?ERROR("Meter: ~p can't read_recharge_num", [Meter]),
                            {false, read_recharge_num_failure}
                    end
            end;
        _ -> 
            false
    end;
send_recharge_(Meter_type, Meter, Cq_weight, Recharge_money, Recharge_num, 0) ->
    false.
        

append_recharge_record(Meter, RechargeRecord) ->
    Recharge_filepath = get_filepath_of_recharge(Meter),
    filelib:ensure_dir(Recharge_filepath),
    write_to_file(Recharge_filepath, RechargeRecord, [append, binary]).

    
write_to_file(FilePath, Content, Modes) ->
    case file:open(FilePath, Modes) of
        {ok, Fd} ->
            io:fwrite(Fd, "~s~n", [Content]),
            file:close(Fd),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.


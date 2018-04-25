-module(analyze_master_control_protocol).

-behaviour(gen_server).

-export([start_link/4]).

-export([init/1, 
         init/4,
         handle_call/3, 
         handle_cast/2, 
         handle_info/2,
         terminate/2, 
         code_change/3
]).

-define(TIMEOUT, 5 * 1000).

-include("analyze.hrl").
-include("analyze_config.hrl").
-include("print.hrl").

-record(state, {
          socket,
          transport,
          data_list = [] :: list()
         }).

start_link(Ref, Socket, Transport, Opts) ->
    proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).

init([]) -> {ok, undefined}.

init(Ref, Socket, Transport, Opts) ->
    ok = proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{active, once}, {send_timeout, ?SOCKET_TIMEOUT}, {exit_on_close, false}]),
    State = #state{
               socket = Socket,
               transport = Transport,
               data_list = []
              },
    gen_server:enter_loop(?MODULE, [], State, ?TIMEOUT).

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, #state{socket = Socket, transport = Transport} = State) ->
    Transport:close(Socket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info({tcp, Socket, Data_Bin}, #state{socket = Socket, transport = Transport, data_list = Data_List} = State) ->
    ok = Transport:setopts(Socket, [{active, once}]),
    {noreply, State#state{data_list = [Data_List, Data_Bin]}, ?TIMEOUT};
    
handle_info({tcp_closed, _Socket}, #state{socket = Socket, data_list = Data_List} = State) ->
    Data_Str = binary_to_list(list_to_binary(Data_List)),
    ?PRINT("MSG:~s~n", [Data_Str]),
    Reply = case string:tokens(Data_Str, "/") of
        [Msg_type, Data_field_str] -> 
            try process_master_control(Msg_type, Data_field_str) of
                ok ->
                    <<"ok">>;
                {error, Msg} ->
                    ?ERROR("master control msg:~p is error:~p~n", [Data_List, Msg]),
                    <<"error">>;
                Other ->
                    ?ERROR("process_master_control return:~p~n", [Other]),
                    <<"error">>
            catch
                Class:Reason ->
                    ?ERROR("master control msg:~p catch exception:~p:~p~n", [Data_List, Class, Reason]),
                    <<"error">>
            end;
        _ ->
            ?ERROR("master control msg:~p is not match", [Data_List]),
            <<"error">>
    end,
    reply(Socket, Reply),
    {stop, normal, State};
handle_info({tcp_error, _, Reason}, #state{socket = _Socket} = State) ->
    {stop, Reason, State};
handle_info(timeout, #state{socket = _Socket} = State) ->
    {stop, normal, State};
handle_info(_Info, State) ->
    {stop, normal, State}.

reply(Socket, Reply) ->
    case gen_tcp:send(Socket, Reply) of
        ok ->
            ok;
        {error, Reason} ->
            ?ERROR("reply gen_tcp:send is error: ~p~n", [Reason])
    end.

process_master_control(Msg_type, Data_field_str) ->
    ?PRINT("Msg_type:~p Data_field_str:~p~n", [Msg_type, Data_field_str]),
    case Msg_type of

        %% 虚拟设备的增删操作
        %% addMeter/0a0001aa7k#002014110119#42435c5573fe#0001000200010001#ac#office_ac
        %% delMeter/0a0001aa7k#002014110119
        "addMeters" ->
            add_meters(Data_field_str);
        "delMeters" ->
            del_meters(Data_field_str);
        "batchUpdateBuild" ->
            batch_update_build(Data_field_str);
        "deleteMetersByGateway" ->
            delete_meters_by_gateway(Data_field_str);

        %% 定时任务的增删操作
        %% addBatchTask/000000000001#2016-09-28 13:50:00#2016-09-28 20:50:00#hz#1111#building=*&meter=150721023750&eqpt_type=*
        "addBatchTask" ->
            analyze_batch_task_server:add_batch_task(Data_field_str);
        %% delBatchTask/000000000001
        "delBatchTask" ->
            analyze_batch_task_server:del_batch_task(Data_field_str);

        %% 周循环任务的增删操作
        %% addCycleTask/002014110119#XXXXXXXXXXXX
        %% delCycleTask/002014110119#XXXXXXXXXXXX
        %% delAllCycleTask/002014110119
        "addCycleTask" ->
            analyze_meter_cycle_task:add_cycle_task(Data_field_str);
        "delCycleTask" ->
            analyze_meter_cycle_task:del_cycle_task(Data_field_str);
        "delAllCycleTask" ->
            analyze_meter_cycle_task:del_all_cycle_task(Data_field_str);

        %% 充值记录
        %% addRechargeRecord/Meter_type#Meter#Recharge_num#Recharge_date#Recharge_time#Recharge_money#Recharge_status
        %% addRechargeRecord/0a0001aa7k#002014110119#8#2016-08-24#09:36:24#00001000#Unknown
        "addRechargeRecord" ->
            add_recharge_record(Data_field_str);

        %% 房间时间表
        %% addTimeTable/0000000001#0001000100020002#0a0001aa7k&ac;0a0001aa8k&lighting#1#2016-08-21#2017-01-20#1&08:00-12:00 13:00-18:00 18:40-21:30;2&08:00-12:00 13:00-18:00 18:40-21:30
        "addTimeTable" ->
            analyze_build_time_table:add_time_table(Data_field_str);

        %% 灯光策略任务下发
        %% addStrategyTask/01020102#000100000000,0a0003ahup,0a00000000000000000000000004000000
        %% addStrategyTask/01020103#000100000000,0a0003ahup,f16332000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
        "addStrategyTask" ->
            add_strategy_task(Data_field_str);
        %% deleteStrategyTask/01020102
        "delStrategyTask" ->
            delete_strategy_task(Data_field_str);
        %% touchStrategyTask/01020101
        "touchStrategyTask" ->
            touch_strategy_task(Data_field_str);

        %% 定额管理
        %% addQuota/99.9#0#building=*&meter=150721023750&eqpt_type=0a0001aa7k
        "addQuota" ->
            analyze_meter_quota_server:add_quota(Data_field_str);
        %% delQuota/0a0001aa7k#150721023750
        "delQuota" ->
            analyze_meter_quota_server:del_quota(Data_field_str);
        Other ->   
            ?ERROR("Module:~p MsgType:~p is not match~n", [?MODULE, Other])
    end.

%%--------------------------------------------------------------------------------------------
%% 处理添加/删除虚拟表
%%--------------------------------------------------------------------------------------------

add_meters(Msg) ->
    MeterInfoList = string:tokens(Msg, ";"),
    Fun = 
        fun(MeterInfo) ->
                case analyze_meter_field:get_meter_field_by_meter_info(MeterInfo) of
                    {ok, MeterField} ->
                        {true, MeterField};
                    {error, Reason} ->
                        ?ERROR("~p is error:~p~n", [MeterInfo, Reason]),
                        false
                end
        end,
    MeterFieldList = lists:filtermap(Fun, MeterInfoList),
    analyze_server:add_meters(MeterFieldList),
    ok.

del_meters(Msg) ->
    MeterInfoList = string:tokens(Msg, ";"),
    Fun = 
        fun(MeterInfo) ->
                case string:tokens(MeterInfo, "#") of
                    [Meter_type, Meter] ->
                        {true, {Meter_type, Meter}};
                    _ ->
                        ?ERROR("del_meter Msg:~p is not match~n", [MeterInfo]),
                        false
                end
        end,
    MeterTypeAndMeterList = lists:filtermap(Fun, MeterInfoList),
    analyze_server:del_meters(MeterTypeAndMeterList),
    ok.

batch_update_build(Msg) ->
    MeterAndBuildInfo = string:tokens(Msg, ";"),
    Fun =
        fun(MeterAndBuild) ->
                case string:tokens(MeterAndBuild, "#") of
                    [Meter_type, Meter, Build_id] ->
                        {true, {Meter_type, Meter, Build_id}};
                     _ ->
                        ?ERROR("update_build Msg:~p is not match~n", [MeterAndBuild]),
                        false
                end
        end,
    MeterTypeAndMeterAndBuildIdList = lists:filtermap(Fun, MeterAndBuildInfo),
    analyze_server:batch_update_build(MeterTypeAndMeterAndBuildIdList),
    ok.

delete_meters_by_gateway(Data_field_str = Gateway) ->
    analyze_server:delete_meters_by_gateway(Gateway),
    ok.

%%--------------------------------------------------------------------------------------------
%% 处理充值记录
%%-------------------------------------------------------------------------------------------- 

add_recharge_record(Data_field_str) ->
    %% 0a0001aa7k#002014110119#8#2016-08-24#09:36:24#00001000#unknown
    case string:tokens(Data_field_str, "#") of
        [Meter_type, Meter, Order_no, Recharge_num, Recharge_date, Recharge_time, Recharge_money, Recharge_status] ->
            case analyze_meter_util:get_running_pid(Meter_type, Meter) of
                {ok, Pid} ->
                    Pid ! {add_recharge_record, Order_no, Recharge_num, Recharge_date, Recharge_time, Recharge_money, Recharge_status};
                {error, Reason} ->
                    ?ERROR("get_running_pid(~p, ~p) is error: ~p~n", [Meter_type, Meter, Reason])
            end;
        _ ->
            ?ERROR("add_recharge_record msg:~p is not match~n", [Data_field_str])
    end.

%%-------------------------------------------------------------------------------------------
%% 策略任务
%%-------------------------------------------------------------------------------------------

add_strategy_task(Data_field_str) ->
    %% 01020102#000100000000,0a0003ahup,0a00000000000000000000000004000000;000100000000,0a0003ahup,f16332000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
    case string:tokens(Data_field_str, "#") of
        [Task_id, Cmd_list_str] ->
            Gateway_cmd_list = get_gateway_cmd_list(Cmd_list_str),
            analyze_strategy_task_server:add_strategy_task(Task_id, Gateway_cmd_list),
            ok;
        _ ->
            ?ERROR("add_strategy_task msg: ~p is not match~n", [Data_field_str]),
            {error, "add_strategy_task msg is not match"}
    end.

get_gateway_cmd_list(Cmd_list_str) ->
    Cmd_str_list = string:tokens(Cmd_list_str, ";"),
    Fun =
        fun(Cmd_str) ->
                case string:tokens(Cmd_str, ",") of
                    [Gateway, Gateway_type, Cmd_data] ->
                        {true, {Gateway, Gateway_type, Cmd_data}};
                    _ ->
                        false
                end
        end,
    lists:filtermap(Fun, Cmd_str_list).

delete_strategy_task(Task_id) ->
    analyze_strategy_task_server:delete_strategy_task(Task_id).

touch_strategy_task(Task_id) ->
    analyze_strategy_task_server:touch_strategy_task(Task_id).
    



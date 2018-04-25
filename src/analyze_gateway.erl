-module(analyze_gateway).

-include("analyze.hrl").
-include("analyze_config.hrl").
-include("batch_task.hrl").
-include("cmd_obj.hrl").
-include("print.hrl").
-include("report.hrl").

-behavisor(gen_server2).

%% API
-export([start_link/1, stop/1]).

%% Gen_server2 callbacks
-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

%% Gen_server2 priorities
-export([
         prioritise_call/4,
         prioritise_cast/3,
         prioritise_info/3
        ]).

-define(SERVER, ?MODULE).

-record(state, {gateway, master_task_queue, slave_task_queue}).

%%%==================================================================
%%% API
%%%==================================================================

%%-------------------------------------------------------------------
%% @doc
%% Start the server
%%
%% @spec start_link(Gateway_Code) -> {ok, Pid} | ignore | {error, Error}
%% @end
start_link(Gateway) ->
    gen_server2:start_link(?MODULE, [Gateway], []).

stop(Pid) ->
    gen_server2:cast(Pid, stop).

%%%==================================================================
%%% gen_server2 callbacks
%%%==================================================================

%%-------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server
%%
%% @spec init(Args) -> {ok, State} | 
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%-------------------------------------------------------------------
init([Gateway]) ->
    erlang:process_flag(trap_exit, true),
    State = #state{gateway = Gateway, master_task_queue = queue:new(), slave_task_queue = queue:new()},
    analyze_gateway_pid:insert(Gateway, self()),
    {ok, State}.

%% The higher the value the higher ther priority.
prioritise_call(_Msg, _From, _Len, _State) ->
    1.

prioritise_cast(_Msg, _Len, _State) ->
    0.

prioritise_info(Msg, _Len, _State) when is_tuple(Msg) andalso 
                                        (element(1, Msg) =:= hour_data  orelse 
                                         element(1, Msg) =:= frozen_data_of_month orelse
                                         element(1, Msg) =:= used_data_of_month) ->
    2;
prioritise_info(_Msg, _Len, _State) ->
    20.

%%-------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) -> 
%%                                      {reply, Reply, State} | 
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc
%%-------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%-------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% 
%% @spec handle_cast(Msg, State) ->
%%                          {noreply, State} |
%%                          {noreply, State, Timeout} |
%%                          {stop, Reason, State}
%% @end
%%-------------------------------------------------------------------
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%-------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) ->
%%                          {noreply, State} |
%%                          {noreply, State, Timeout} |
%%                          {stop, Reason, State}
%% @end
%%-------------------------------------------------------------------

%% 灯光策略任务
handle_info({strategy_task, From, ?LIGHTING_TYPE = Meter_type, Gateway_cmd_obj, TaskId, ReissuedType}, State) ->
    Cq_weight = 100,
    Result = strategy_task_repeat_exec(Meter_type, Gateway_cmd_obj, Cq_weight, ReissuedType, 
                                       exec_middle_cmd(Gateway_cmd_obj, Cq_weight)),
    From ! {strategy_task_result, TaskId, Result},
    {noreply, State};

%% 中央空调策略任务
handle_info({strategy_task, From, ?CENTRAL_AC_TYPE = Meter_type, Gateway_cmd_obj_list, TaskId, ReissuedType}, State) 
  when is_list(Gateway_cmd_obj_list) ->
    Cq_weight = 100,
    Fun = 
        fun(Gateway_cmd_obj, {ok, _}) ->
                strategy_task_repeat_exec(Meter_type, Gateway_cmd_obj, Cq_weight, ReissuedType, 
                                          exec_middle_cmd(Gateway_cmd_obj, Cq_weight));
           (GatewayCmdList, Result) ->
                Result
        end,
    Result = lists:foldl(Fun, {ok, ok}, Gateway_cmd_obj_list),
    From ! {strategy_task_result, TaskId, Result},
    {noreply, State};

%% 房间任务
handle_info({room_task, From, Room_task}, State) ->
    {Meter, Meter_type, Operation_type, Operation_argv} = Room_task,
    Cq_weight = get_cq(Meter_type, Meter), 
    case exec_task(Meter, Meter_type, Operation_type, Operation_argv, Cq_weight) of
        true -> From ! {room_task_result, Room_task, true};
        false -> 
            timer:sleep(5*1000),
            case exec_task(Meter, Meter_type, Operation_type, Operation_argv, Cq_weight) of
                true -> From ! {room_task_result, Room_task, true};
                false -> From ! {room_task_result, Room_task, false}
            end  
    end,
    {noreply, State};

%% 批量任务
handle_info({batch_task, Task_obj_Bin}, State) ->
    #state {
        gateway = Gateway,
        master_task_queue = Master_task_queue,
        slave_task_queue = Slave_task_queue
    } = State,
    Task_obj_list = binary_to_term(Task_obj_Bin),
    ?PRINT("Task_obj_list:~p of Gateway:~p ~n", [Task_obj_list, Gateway]),
    {Success_list_1, Failure_list_1} = exec_tasks(Task_obj_list),
    {Success_list_2, Failure_list_2} = exec_tasks(Failure_list_1),
    ?ERROR("Success_list_1:~p Success_list_2:~p~n",[Success_list_1, Success_list_2]),
    {noreply, State};

%% 计量表任务
handle_info({meter_task, From, Meter_task}, State) ->
    {Meter, Meter_type, Operation_type, Operation_argv} = Meter_task,
    Cq_weight = get_cq(Meter_type, Meter), 
    case exec_task(Meter, Meter_type, Operation_type, Operation_argv, Cq_weight) of
        true -> From ! {meter_task_result, Meter_task, true};
        false -> From ! {meter_task_result, Meter_task, false}
            %case exec_task(Meter, Meter_type, Operation_type, Operation_argv, Cq_weight) of
             %   true -> From ! {meter_task_result, Meter_task, true};
              %  false -> From ! {meter_task_result, Meter_task, false}
            %end  
    end,
    {noreply, State};

%% 整点小时数据
handle_info({hour_data, From, Meter_type, Meter, Cmd_obj_list}, State) ->
    Cq_weight = get_cq(Meter_type, Meter), 
    Fun = fun
        (Cmd_obj) ->
            Result = 
                      case exec_middle_cmd(Cmd_obj, Cq_weight) of
                          {ok, Result_str} -> {ok, Result_str};
                          _ -> false
                              %case exec_middle_cmd(Cmd_obj, Cq_weight) of
                               %   {ok, Result_str_1} -> {ok, Result_str_1};
                                %  false -> 
                                 %     false
                              %end
                      end,
            From ! {hour_data_result, Result}
    end,
    lists:foreach(Fun, Cmd_obj_list),
    {noreply, State};

%% 月度冻结数据
handle_info({frozen_data_of_month, From, Meter_type, Meter, Cmd_obj_list}, State) ->
    Cq_weight = get_cq(Meter_type, Meter), 
    Fun = fun
        (Cmd_obj) ->
            Result = case exec_middle_cmd(Cmd_obj, Cq_weight) of
                {ok, Result_str} -> {ok, Result_str};
                _ -> 
                   false          
                    %case exec_middle_cmd(Cmd_obj, Cq_weight) of
                     %   {ok, Result_str_1} -> {ok, Result_str_1};
                      %  _ -> 
                       %     false
                    %end
            end,
            From ! {frozen_data_of_month_result, Result}
    end,
    lists:foreach(Fun, Cmd_obj_list),
    {noreply, State};

%% 月度使用数据
handle_info({used_data_of_month, From, Meter_type, Meter, Cmd_obj_list}, State) ->
    Cq_weight = get_cq(Meter_type, Meter), 
    Fun = fun
        (Cmd_obj) ->
            Result = case exec_middle_cmd(Cmd_obj, Cq_weight) of
                {ok, Result_str} -> {ok, Result_str};
                _ ->
                    false         
                    %case exec_middle_cmd(Cmd_obj, Cq_weight) of
                     %   {ok, Result_str_1} -> {ok, Result_str_1};
                      %  false -> 
                       %     false
                    %end
            end,
            From ! {used_data_of_month_result, Result}
    end,
    lists:foreach(Fun, Cmd_obj_list),
    {noreply, State};

handle_info({report_come, ?STATUSMSG, Gateway_type, Gateway, Status}, State) ->
    ?PRINT("report_come Gateway_type:~p Gateway:~p Status:~p~n", [Gateway_type, Gateway, Status]),
    analyze_gateway_status:insert(Gateway, Status),
    {noreply, State};

handle_info(timeout, State) ->
    ?DEBUG("timeout .......~n", []),
    {noreply, State};

handle_info({'EXIT', _Pid, Reason}, State) ->
    ?PRINT("Reason : ~p~n", [Reason]),
    {stop, Reason, State};

handle_info(Info, State) ->
    ?PRINT("Info : ~p~n", [Info]),
    {noreply, State}.

%%-------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server2 when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. when it returns, the gen_server2 terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%-------------------------------------------------------------------
terminate(_Reason, State = #state{gateway = Gateway}) ->
    analyze_gateway_pid:delete(Gateway),
    analyze_gateway_status:delete(Gateway),
    ok.

%%-------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% 
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%-------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

strategy_task_repeat_exec(_Meter_type, Gateway_cmd_obj, _Cq_weight, {_Interval, 0}, Result) ->
    Result;
strategy_task_repeat_exec(?LIGHTING_TYPE = Meter_type, Gateway_cmd_obj, Cq_weight, {Interval, Repeat}, Result) ->
    timer:sleep(Interval*1000),    
    strategy_task_repeat_exec(Meter_type, Gateway_cmd_obj, Cq_weight, {Interval, Repeat-1}, 
                              exec_middle_cmd(Gateway_cmd_obj, Cq_weight));
strategy_task_repeat_exec(?CENTRAL_AC_TYPE, Gateway_cmd_obj, Cq_weight, {Interval, Repeat}, {ok, _} = Result) ->
    Result;
strategy_task_repeat_exec(?CENTRAL_AC_TYPE = Meter_type, Gateway_cmd_obj, Cq_weight, {Interval, Repeat}, Result) ->
    timer:sleep(Interval*1000),    
    strategy_task_repeat_exec(Meter_type, Gateway_cmd_obj, Cq_weight, {Interval, Repeat-1}, 
                              exec_middle_cmd(Gateway_cmd_obj, Cq_weight)).


exec_tasks(Task_obj_list) ->
    Fun = 
        fun(Task_obj, {Success_list_tmp, Failure_list_tmp}) ->
                #task_obj{
                   task_id = Task_id,
                   meter = Meter,
                   meter_type = Meter_type, 
                   operation_type = Operation_type, 
                   operation_argv = Operation_argv
                  } = Task_obj,
                Cq_weight = get_cq(Meter_type, Meter), 
                case exec_task(Meter, Meter_type, Operation_type, Operation_argv, Cq_weight) of
                    true -> {[Task_obj | Success_list_tmp], Failure_list_tmp};
                    false -> {Success_list_tmp, [Task_obj | Failure_list_tmp]}
                end
        end,
    {Success_list, Failure_list} = lists:foldl(Fun, {[], []}, Task_obj_list),
    {Success_list, Failure_list}.

exec_task(Meter, Meter_type, Operation_type, Operation_argv, Cq_weight) ->
    %% 前置操作
    Before_operation_cmd_obj_list = get_before_operation_cmd_objs(Meter, Meter_type, Operation_type, Operation_argv),
    Cmd_obj = get_cmd_obj(Meter, Meter_type, Operation_type, Operation_argv),
    Fun = fun
        (Cmd_obj_tmp, Prev_result) ->
            case Prev_result of
                false ->
                    false;
                true ->
                    case is_exec_success(Cmd_obj_tmp, Cq_weight) of
                        false -> false;
                        true -> true    
                    end
            end
    end,
    Is_exec_success = lists:foldl(Fun, true, lists:append(Before_operation_cmd_obj_list, [Cmd_obj])),
    Is_exec_success.

is_exec_success(Cmd_obj, Cq_weight) ->
    case exec_middle_cmd(Cmd_obj, Cq_weight) of
        {ok, Result_str} -> true;
        _ -> false   
    end.

%% return: {error, Reason} | {ok, Result} | {false, Result} | false
exec_middle_cmd(Cmd_obj, Cq_weight) ->    
    case send_cmd_to_middleware:send(Cmd_obj, Cq_weight) of
        {ok, Return_data} ->
            send_cmd_to_middleware:get_result(Return_data);
        {error, Reason} ->
            {error, Reason}
    end.

get_before_operation_cmd_objs(Meter, Meter_type = ?AC_TYPE, ?TZ, Operation_argv) ->
    Lists = [
        {"yckz", "hwfs", "03F002DE01F5023206040137247411233104005002714E002000200010769F0136247511233104007002E849002000005040"}
    ],
    Fun = fun
        ({Cmd_type, Cmd_id, Cmd_data})->
            #cmd_obj{
                eqpt_type = Meter_type, 
                eqpt_id_code  = Meter,
                cmd_type = Cmd_type,
                cmd_id = Cmd_id,
                cmd_data = Cmd_data
            }
    end,
    lists:map(Fun, Lists);
get_before_operation_cmd_objs(Meter, Meter_type, Operation_type, Operation_argv) ->
    [].
    
get_cmd_obj(Meter, Meter_type, Operation_type, Operation_argv) ->
    {Cmd_type, Cmd_id, Cmd_data} = get_cmd_info(Meter_type, Operation_type, Operation_argv),
    #cmd_obj{
        eqpt_type = Meter_type, 
        eqpt_id_code  = Meter,
        cmd_type = Cmd_type,
        cmd_id = Cmd_id,
        cmd_data = Cmd_data
    }.

%%-----------------------------------------------------------------
%% 跳闸操作
%%-----------------------------------------------------------------

%% 空调跳闸，cmd_data为""
get_cmd_info(?AC_TYPE, ?TZ, _Operation_argv) ->
    {"yckz", "tz", ""};
%% 中央空调跳闸，cmd_data为"55"
get_cmd_info(?CENTRAL_AC_TYPE, ?TZ, _Operation_argv) ->
    {"yckz", "yckg", "55"};
%% 插座跳闸，四路面板 cmd_data为"1111"
get_cmd_info(Type, ?TZ, _Operation_argv) when 
      (Type =:= ?SOCKET_TYPE) orelse (Type =:= ?FOUR_WAY_SWITCH_TYPE) ->
    {"yckz", "tz", "0f"};

%%-----------------------------------------------------------------
%% 合闸操作
%%-----------------------------------------------------------------

%% 空调合闸，cmd_data为""
get_cmd_info(?AC_TYPE, ?HZ, _Operation_argv) ->
    {"yckz", "hz", ""};
%% 中央空调合闸，cmd_data为"aa"
get_cmd_info(?CENTRAL_AC_TYPE, ?HZ, _Operation_argv) ->
    {"yckz", "yckg", "aa"};
%% 插座合闸，cmd_data为"1111"
get_cmd_info(Type, ?HZ, _Operation_argv) when 
      (Type =:= ?SOCKET_TYPE) orelse (Type =:= ?FOUR_WAY_SWITCH_TYPE) ->
    {"yckz", "hz", "0f"};

get_cmd_info(_, _, _) ->
    {?NULL, ?NULL, ?NULL}.

get_cq(Meter_type, Meter) ->
    analyze_meter_util:get_cq(Meter_type, Meter).    

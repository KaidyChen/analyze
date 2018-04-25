-module(analyze_lighting_strategy_server).

-behaviour(gen_server).

-include("analyze.hrl").
-include("analyze_store.hrl").
-include("print.hrl").
-include("cmd_obj.hrl").

-export([start_link/0]).
-export([
         add_lighting_strategy/1,
         touch_lighting_strategy/1,
         delete_lighting_strategy/1,
         get_strategy_list/0
        ]).

-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-define(SERVER, ?MODULE).
-define(TIMEOUT, 60*1000).

-define(DGKZ, "dgkz").
-define(DK, "dk").

-record(state, {
          selfPid,
          timer
         }).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add_lighting_strategy(Lighting_strategy_tuple) ->
    gen_server:cast(?SERVER, {add_lighting_strategy, Lighting_strategy_tuple}).

touch_lighting_strategy(TaskId) ->
    gen_server:call(?SERVER, {touch_lighting_strategy, string:to_lower(TaskId)}, 5*1000).

delete_lighting_strategy(TaskId) ->
    gen_server:cast(?SERVER, {delete_lighting_strategy, string:to_lower(TaskId)}).

get_strategy_list() ->
    gen_server:call(?SERVER, {get_strategy_list}).

init([]) ->
    State = #state{selfPid = self()},
    {ok, State, 0}.

handle_call({get_strategy_list}, _From, State) ->
    Reply = analyze_lighting_strategy_store:get_strategy_list(),
    {reply, Reply, State};
handle_call({touch_lighting_strategy, TaskId}, _From, State) ->
    Reply = 
        case analyze_lighting_strategy_store:lookup(TaskId) of
            {ok, Lighting_strategy_rd} ->
                ?PRINT("~p ~p~n", [TaskId, Lighting_strategy_rd]),
                ok = send_to_gateway(Lighting_strategy_rd);
            _ ->
                {error, "not found the taskId"}
        end,
    {reply, Reply, State};
    
handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast({add_lighting_strategy, Lighting_strategy_tuple_List}, State) when is_list(Lighting_strategy_tuple_List) ->
    ?PRINT("~p~n", [Lighting_strategy_tuple_List]),
    [handle_add_lighting_strategy(Lighting_strategy_tuple) || Lighting_strategy_tuple <- Lighting_strategy_tuple_List],
    {noreply, State};
handle_cast({add_lighting_strategy, Lighting_strategy_tuple}, State) ->
    ?PRINT("~p~n", [Lighting_strategy_tuple]),
    handle_add_lighting_strategy(Lighting_strategy_tuple),
    {noreply, State};
handle_cast({touch_lighting_strategy, TaskId}, State) ->    
    handle_touch_lighting_strategy(TaskId),
    {noreply, State};
handle_cast({delete_lighting_strategy, TaskIdList}, State) when is_list(TaskIdList) ->
    [handle_delete_lighting_strategy(TaskId) || TaskId <- TaskIdList],
    {noreply, State};
handle_cast({delete_lighting_strategy, TaskId}, State) ->
    handle_delete_lighting_strategy(TaskId),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, State = #state{selfPid = SelfPid}) ->
    load_lighting_strategy_task(),
%    ?PRINT("TaskIds:~p~n", [erlang:get_keys()]),
    Datetime = ?HELP:datetime_now(),
    NextDatetime = ?HELP:addSecond(Datetime, 60),
    NewState = State#state{timer = erlang:start_timer(?TIMEOUT, SelfPid, NextDatetime)},
    handle_timer_lighting_strategy(Datetime),

    {noreply, NewState};
handle_info({timeout, TimerRef, Datetime}, State = #state{selfPid = SelfPid}) ->
    erlang:cancel_timer(TimerRef, [{async, true}]),
    NextDatetime = ?HELP:addSecond(Datetime, 60),
    NewState = State#state{timer = erlang:start_timer(?TIMEOUT, SelfPid, NextDatetime)},
    handle_timer_lighting_strategy(Datetime),
    {noreply, NewState};

handle_info({strategy_task_result, _TaskId, {ok, _ResultMsg} = _Result}, State) ->
    {noreply, State};

handle_info({strategy_task_result, TaskId, {false, ResultMsg} = Result}, State) ->
    Msg_type = "lighting_strategy_task_result",
    Msg_body = string:join([TaskId, ResultMsg], "#"),
    analyze_push_service:push(Msg_type, Msg_body),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {noreply, State}.

load_lighting_strategy_task() ->
    analyze_lighting_strategy_store:load().

handle_add_lighting_strategy({TaskId, StrategyType, ReissuedType, DataList}) ->
    Lighting_strategy_rd = #lighting_strategy_rd{
                              task_id = string:to_lower(TaskId),
                              strategy_type = StrategyType,
                              reissued_type = ReissuedType,
                              data_list = DataList
                             },
    analyze_lighting_strategy_store:insert(Lighting_strategy_rd);
handle_add_lighting_strategy({TaskId, StrategyType, ValidityStart, ValidityEnd, 
                              CycleMode, WeekList, TimeList, ReissuedType, DataList}) ->
        Lighting_strategy_rd = #lighting_strategy_rd{
                                  task_id = string:to_lower(TaskId),
                                  strategy_type = StrategyType,
                                  validity_start = ValidityStart,
                                  validity_end = ValidityEnd,
                                  cycle_mode = CycleMode,
                                  week_list = WeekList,
                                  time_list = TimeList,
                                  reissued_type = ReissuedType,
                                  data_list = DataList
                             },
    analyze_lighting_strategy_store:insert(Lighting_strategy_rd).

handle_touch_lighting_strategy(TaskId) ->
    ?PRINT("Touch taskId:~p~n", [TaskId]),
    case analyze_lighting_strategy_store:lookup(TaskId) of
        {ok, Lighting_strategy_rd} ->
            ?PRINT("~p ~p~n", [TaskId, Lighting_strategy_rd]),
            send_to_gateway(Lighting_strategy_rd);
        _ ->
            ok
    end.

handle_delete_lighting_strategy(TaskId) ->
    analyze_lighting_strategy_store:delete(TaskId).
    
handle_timer_lighting_strategy(Datetime = {Date, Time}) ->
    TimerStrategyList = get_timer_strategy_list(),
    Fun = 
        fun(TimerLightingStrategy) ->
                case check_condition(Date, Time, TimerLightingStrategy) of
                    true ->
                        send_to_gateway(TimerLightingStrategy);
                    _ ->
                        ok
                end
        end,
    lists:foreach(Fun, TimerStrategyList),
%    ?ERROR("task Datetime: ~p~n", [Datetime]),
%    ?PRINT("TimerStrategy~p~n", [TimerStrategyList]),
    ok.

check_condition(Date = {Year, Month, Day}, Time = {Hour, Minute, _}, TimerLightingStrategy) ->
    #lighting_strategy_rd{
       task_id = TaskId,
       strategy_type = StrategyType,
       validity_start = ValidityStart,
       validity_end = ValidityEnd,
       cycle_mode = CycleMode,
       week_list = WeekList,
       time_list = TimeList,
       reissued_type = ReissuedType
      } = TimerLightingStrategy,
     check_time(Hour, Minute, TimeList) andalso check_date(Date, ValidityStart, ValidityEnd) 
        andalso check_week(Date, WeekList, CycleMode).
    
check_time(Hour, Minute, [{Hour, Minute} | T]) -> 
    true;
check_time(Hour, Minute, [_ | T]) ->
    check_time(Hour, Minute, T);
check_time(_, _, []) ->
    false.


check_date(Date, ValidityStart, ValidityEnd) ->
    (ValidityStart =< Date) andalso (Date =< ValidityEnd).

check_week(_Date, _Week, ?ONLY_ONCE) -> 
    true;
check_week(Date, _WeekList, CycleMode) when (CycleMode =:= ?WORK_DAY) 
                                            orelse (CycleMode =:= ?NOT_WORK_DAY)  ->
    case CycleMode of
        ?WORK_DAY ->
            not (analyze_util:is_holiday(Date));
        ?NOT_WORK_DAY ->
            analyze_util:is_holiday(Date)
    end;
check_week(Date, WeekList, ?WEEK_LIST) -> 
    Day_of_the_week = calendar:day_of_the_week(Date),
    lists:member(Day_of_the_week, WeekList).

send_to_gateway(Lighting_strategy_rd) ->
    #lighting_strategy_rd{
       task_id = TaskId,
       data_list = DataList,
       reissued_type = ReissuedType
      } = Lighting_strategy_rd,
    send_to_gateway(DataList, TaskId, ReissuedType).

send_to_gateway([{?LIGHTING_TYPE = MeterType, GatewayCmdList} | DataList], TaskId, ReissuedType) ->
    Fun = 
        fun(GatewayCmd = {Gateway, GatewayType, CmdData}) ->                                
                case analyze_gateway_util:get_gateway_pid_by_gateway(Gateway) of
                    {ok, Pid} ->
                        GatewayCmdObj = gen_lighting_cmd_obj(Gateway, GatewayType, CmdData),
                        Pid ! {strategy_task, self(), MeterType, GatewayCmdObj, TaskId, ReissuedType};                    
                    {error, Reason} ->
                        ?ERROR("pid of gateway:~p is ~p ~n", [GatewayCmd, Reason])
                end
        end,
    lists:foreach(Fun, GatewayCmdList),
    ?PRINT("~p~n", [DataList]),
    send_to_gateway(DataList, TaskId, ReissuedType);
send_to_gateway([{?CENTRAL_AC_TYPE = MeterType, MeterCmdList} | DataList], TaskId, ReissuedType) ->
    ?PRINT("MeterCmdList:~p~n", [MeterCmdList]),
    Fun = 
        fun({Meters, CmdOptList}) ->
                Fun1 = 
                    fun(MeterBin) ->
                            Meter = binary_to_list(MeterBin),
                            case get_gateway_pid_by_meter(MeterType, Meter) of
                                {ok, Pid} ->
                                    GatewayCmdObjList = gen_cmd_obj_by_cmd_opt_list(MeterType, Meter, CmdOptList),
                                    Pid ! {strategy_task, self(), MeterType, GatewayCmdObjList, TaskId, ReissuedType};
                                {error, Reason} ->
                                    ?ERROR("meter: ~p  pid of gateway is ~p ~n", [Meter, Reason])
                            end
                    end,
                lists:foreach(Fun1, Meters)
        end,
    lists:foreach(Fun, MeterCmdList),
    send_to_gateway(DataList, TaskId, ReissuedType);
send_to_gateway([], _, _) ->
    ok.

get_timer_strategy_list() ->
    analyze_lighting_strategy_store:get_timer_strategy_list().

gen_lighting_cmd_obj(Gateway, GatewayType, CmdData) ->
    #cmd_obj{
       eqpt_type = GatewayType,
       eqpt_id_code = Gateway,
       cmd_type = ?DGKZ,
       cmd_id = ?DK,
       cmd_data = CmdData
      }.

gen_common_cmd_obj(MeterType, Meter, CmdType, CmdId, CmdData) ->
    #cmd_obj{
       eqpt_type = MeterType,
       eqpt_id_code = Meter,
       cmd_type = CmdType,
       cmd_id = CmdId,
       cmd_data = CmdData
      }.

gen_cmd_obj_by_cmd_opt_list(MeterType, Meter, CmdOptList) ->
    gen_cmd_obj_by_cmd_opt_list(MeterType, Meter, CmdOptList, []).

gen_cmd_obj_by_cmd_opt_list(MeterType, Meter, [CmdOpt | CmdOptList], CmdObjList) ->
    CmdType = binary_to_list(proplists:get_value(<<"cmdType">>, CmdOpt)),
    CmdId = binary_to_list(proplists:get_value(<<"cmdId">>, CmdOpt)),
    CmdData = binary_to_list(proplists:get_value(<<"cmdData">>, CmdOpt, <<>>)),
    CmdObj = gen_common_cmd_obj(MeterType, Meter, CmdType, CmdId, CmdData),
    gen_cmd_obj_by_cmd_opt_list(MeterType, Meter, CmdOptList, [CmdObj | CmdObjList]);
gen_cmd_obj_by_cmd_opt_list(_, _, [], CmdObjList) ->
    CmdObjList.
    
get_gateway_pid(Gateway) ->
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

get_gateway_pid_by_meter(MeterType, Meter) ->
    case analyze_meter_field_store:get_gateway(MeterType, Meter) of
        {ok, Gateway} ->
            case analyze_gateway_util:get_gateway_pid_by_gateway(Gateway) of
                {ok, Pid} ->
                    {ok, Pid};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, What} ->
            {error, What}
    end.
            
       








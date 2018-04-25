-module (analyze_build_room).

-include("analyze.hrl").
-include("analyze_config.hrl").
-include("print.hrl").
-include("time_table.hrl").

-behavisor(gen_server).

%% API
-export([start_link/1]).

%% Gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
]).

-record(state, {
    room_id, 
    time_table_timer
}).

-define(TIMER_INTERVAL, 1 * 60 * 1000).
-define(TIMER_MSG, check_time_table).

-define(START_TIME_FLAG, 0).
-define(END_TIME_FLAG, 1).

%%%==================================================================
%%% API
%%%==================================================================

start_link(Room_id) ->
    gen_server:start_link(?MODULE, [Room_id], []).

%%%==================================================================
%%% gen_server callbacks
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
init([Room_id]) ->
    erlang:process_flag(trap_exit, true),
    analyze_build_pid:insert(Room_id, self()),
    State = #state{
        room_id = Room_id
    },
    {ok, State, 0}.

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
handle_info(timeout, State = #state{room_id = Room_id}) ->
    DateTime = ?HELP:datetime_now(),
    NewState = State#state{
                 time_table_timer = check_time_table_timer(DateTime)
                },
    handle_time_table(Room_id, DateTime),
    {noreply, NewState};

handle_info({timeout, TimerRef, DateTime}, State = #state{room_id = Room_id}) ->
    ?PRINT("~p~n", [DateTime]),
    erlang:cancel_timer(TimerRef),
    NewState = State#state{
        time_table_timer = check_time_table_timer(DateTime)
    },
    handle_time_table(Room_id, DateTime),
    {noreply, NewState};

handle_info({room_task_result, Room_task, Result}, State) ->
    {Meter, Meter_type, Operation_type, Operation_argv} = Room_task,
    ?ERROR("Meter:~p Meter_type:~p Operation_type:~p Operation_argv:~p result is ~p.~n", [Meter, Meter_type, Operation_type, Operation_argv, Result]),
    {noreply, State};

handle_info({'EXIT', _From, Reason}, State) ->
    {stop, Reason, State};

handle_info(_Info, State) ->
    
    {noreply, State}.

%%-------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. when it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%-------------------------------------------------------------------
terminate(_Reason, State = #state{room_id = Room_id}) ->
    analyze_build_pid:delete(Room_id),
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


check_time_table_timer(DateTime) ->
    Interval = ?TIMER_INTERVAL,
    erlang:start_timer(Interval, self(), ?HELP:addSecond(DateTime, 60)).

get_time_table_list_of_room(Room_id, Now) ->
    Level_0 = 0,
    Level_1 = 1,
    Level_0_time_table_list = get_effective_date_time_table_list(Room_id, Level_0, Now),
    Level_1_time_table_list = get_effective_date_time_table_list(Room_id, Level_1, Now),
    Fun = fun
        (#time_table{type_and_labels_of_meter = Type_and_labels_of_meter_level_1} = Level_1_time_table, Level_0_time_table_list_tmp) ->
            Pred = fun
                %% Level_0_time_table_list存在与level_1中相同的设备类型的课程表，要剔除
                (#time_table{type_and_labels_of_meter = Type_and_labels_of_meter_level_0} = Level_0_time_table) when (Type_and_labels_of_meter_level_1 =:= Type_and_labels_of_meter_level_0) ->
                    false;
                (_) ->
                    true
            end,
            lists:filter(Pred, Level_0_time_table_list_tmp)
    end,
    New_level_0_time_table_list = lists:foldl(Fun, Level_0_time_table_list, Level_1_time_table_list),
    % ?ERROR("Level_0_time_table_list:~p~n", [Level_0_time_table_list]),
    % ?ERROR("New_level_0_time_table_list:~p~n", [New_level_0_time_table_list]),
    lists:merge(Level_1_time_table_list, New_level_0_time_table_list).
    
%% 获取当前处于有效期的课程表
get_effective_date_time_table_list(Room_id, Level, Now) ->
    case analyze_build_time_table_store:lookup_by_room_id_and_level(Room_id, Level) of
        {ok, Time_table_list} ->
            {Date, _} = Now,
            Pred = fun
                (Time_table) ->
                    #time_table{
                        validity_date_start = Validity_date_start,
                        validity_date_end = Validity_date_end
                    } = Time_table,
                    (Validity_date_start =< Date) andalso (Date =< Validity_date_end)
            end,
            lists:filter(Pred, Time_table_list);
        {error, _} ->
            []
    end.

handle_time_table(Room_id, DateTime) ->
    case get_time_table_list_of_room(Room_id, DateTime) of
        [] -> ok;
        Time_table_list ->            
            handle_time_table(Room_id, Time_table_list, DateTime)
    end.

handle_time_table(Room_id, Time_table_list, Now) ->
    ?PRINT("Time_table_list:~p~n", [Time_table_list]),
    case get_operation_type_and_Type_and_labels_of_meter_list(Time_table_list, Now) of
        [] -> ok;
        OperationType_Type_and_labels_of_meter_list ->
            case analyze_build_meter:lookup(Room_id) of
                {ok, Meter_type_and_meter_list} ->
                    ?PRINT("meter_list:~p~n", [Meter_type_and_meter_list]),
                    Fun = 
                        fun
                            ({Operation_type, Type_and_labels_of_meter}) ->
                                case get_eligibility_meter_list(Meter_type_and_meter_list, Type_and_labels_of_meter) of
                                    [] -> false;
                                    Eligibility_meter_list -> 
                                        {true, {Operation_type, Eligibility_meter_list}}
                                end
                        end,
                    Operation_type_and_meter_field_list = lists:filtermap(Fun, OperationType_Type_and_labels_of_meter_list),
                    ?PRINT("Operation_list:~p~n", [Operation_type_and_meter_field_list]),
                    send_to_gateway_exec(Operation_type_and_meter_field_list);
                _ ->
                    ok
            end
    end.

get_operation_type_and_Type_and_labels_of_meter_list(Time_table_list, Now) ->
    {Date, {Hour, Minute, _}} = Now,   
    Day_of_the_week = calendar:day_of_the_week(Date),
    Fun = fun
        (Time_table) ->
            #time_table{
                key = Key,
                type_and_labels_of_meter = Type_and_labels_of_meter,
                holiday_mode = Holiday_mode,
                time_list = Time_list
            } = Time_table,
            case is_exec_holiday_mode(Date, Holiday_mode) of
                true ->
                    case lists:keyfind(Day_of_the_week, 1, Time_list) of
                        false ->
                            false;
                        {Day_of_the_week, Time_start_end_list} ->
                            case check_time_list(Time_start_end_list, Hour, Minute) of
                                false -> false;
                                {true, Operation_type} ->
                                    {true, {Operation_type, Type_and_labels_of_meter}}
                            end
                    end;
                _ ->
                    false
            end
    end,
    lists:filtermap(Fun, Time_table_list).


is_exec_holiday_mode(Date, Holiday_mode) ->
    case Holiday_mode of
        0 -> true;
        _ ->
            case analyze_util:is_holiday(Date) of
                false -> true;
                true -> false   %% 今天是节假日不执行
            end
    end.

check_time_list([Time_start_end | T], Hour, Minute) ->
    case Time_start_end of
        {{Hour, Minute}, _} ->
            {true, ?HZ};
        {_, {Hour, Minute}} ->
            {true, ?TZ};
        _ ->
            check_time_list(T, Hour, Minute)
    end;
check_time_list([], _Hour, _Minute) ->
    false.
    
get_eligibility_meter_list(Meter_type_and_meter_list, Type_and_labels_of_meter) ->
    Len = length(Type_and_labels_of_meter),
    Pred = 
        fun({Meter_type, Meter}) ->
                case analyze_meter_field_store:lookup(Meter_type, Meter) of
                    {ok, Meter_field} ->                        
                        Slave_label = analyze_meter_field:get_slave_label_by_meter_field(Meter_field),                        
                        case lists:member(Meter_type, Type_and_labels_of_meter) of
                            true -> 
                                case {(Len =:= 1), lists:member(Slave_label, Type_and_labels_of_meter)}  of
                                    {true, _} -> 
                                        {true, Meter_field};
                                    {false, true} ->
                                        {true, Meter_field};
                                    _ ->
                                        false
                                end;
                            false ->
                                false
                        end;
                    _ ->
                        false                  
                end
        end,
    lists:filtermap(Pred, Meter_type_and_meter_list).

send_to_gateway_exec([H | T]) ->
    {Operation_type, Meter_field_list} = H,
    Fun = 
        fun(Meter_field) ->
                {Meter_type, Meter} = analyze_meter_field:get_meter_type_and_meter_by_meter_field(Meter_field),
                Gateway = analyze_meter_field:get_gateway_by_meter_field(Meter_field),
                Operation_argv = " ",
                Room_task = {Meter, Meter_type, Operation_type, Operation_argv},
                send_to_gateway_exec_(Gateway, Room_task)
        end,
    lists:foreach(Fun, Meter_field_list),
    send_to_gateway_exec(T);
send_to_gateway_exec([]) ->
    ok.

send_to_gateway_exec_(Gateway, Room_task) ->
    case analyze_gateway_pid:lookup(Gateway) of
        {ok, Pid} ->
            case erlang:is_process_alive(Pid) of
                true ->
                    Pid ! {room_task, self(), Room_task};
                false ->
                    ?ERROR("gateway:~p process:~p is not alive~n", [Gateway, Pid])
            end,
            ok;
        {error, _} ->
            ?ERROR("not found pid of gateway:~p~n", [Gateway]),
            ok
    end.

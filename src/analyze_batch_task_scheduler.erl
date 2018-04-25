-module(analyze_batch_task_scheduler).

-behaviour(gen_server).

-include("analyze.hrl").
-include("analyze_config.hrl").
-include("batch_task.hrl").
-include("print.hrl").

%% Public API
-export([start_link/0]).

-export([run_batch_task_list/1]).

%% gen_server callback API
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
]).

-record(state, {}).

-define(SERVER, ?MODULE).

%% API

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

run_batch_task_list(Batch_task_list) ->
    gen_server:call(?SERVER, {run_batch_task_list, Batch_task_list}).

init([]) ->
    State = #state{},
    {ok, State}.

handle_call({run_batch_task_list, Batch_task_list}, From, State) ->
    ?PRINT("run_batch_task_list:~p~n", [Batch_task_list]),
    do_run_batch_task_list(Batch_task_list),
    Reply = ok,
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.  

code_change(_OldVersion, State, _Extra) -> {ok, State}. 


do_run_batch_task_list([Batch_task | T]) ->
    {M, C} = analyze_meter_field_store:get_meter_field_list(10000),
    #batch_task{
       task_id = Task_id, 
       operation_type = Operation_type, 
       operation_argv = Operation_argv, 
       condition = Condition_str
      } = Batch_task,
    Condition_field = analyze_util:parse_condition(Condition_str),
    do_run_batch_task({M, C}, Task_id, Operation_type, Operation_argv, Condition_field),
    do_run_batch_task_list(T);
do_run_batch_task_list(_) ->
    ok.

do_run_batch_task({[], _}, _, _, _, _) ->
    ok;
do_run_batch_task({M, C}, Task_id, Operation_type, Operation_argv, Condition_field) ->
    MatchConditionList = analyze_util:get_meter_field_list_by_condition(Condition_field, lists:flatten(M)),
    case MatchConditionList of
        [] -> ok;
        New_meter_field_list ->
            Fun = 
                fun
                    (Meter_field, Map) ->
                        {Meter_type, Meter} = analyze_meter_field:get_meter_type_and_meter_by_meter_field(Meter_field),
                        Gateway = analyze_meter_field:get_gateway_by_meter_field(Meter_field),
                        Task_obj = 
                            #task_obj{
                               task_id = Task_id,
                               meter = Meter,
                               meter_type = Meter_type, 
                               operation_type = Operation_type, 
                               operation_argv = Operation_argv
                              },
                        update_map(Gateway, Task_obj, Map)
                end,
            NewMap = lists:foldl(Fun, maps:new(), New_meter_field_list),
            send_to_gateway(NewMap)
    end,
    {NewM, NewC} = analyze_meter_field_store:get_meter_field_list(C),
    %?PRINT("New ~p~n~p~n", [NewM, NewC]),
    do_run_batch_task({NewM, NewC}, Task_id, Operation_type, Operation_argv, Condition_field).

group_gateway_task_obj(Meter_field_list, Batch_task_list) ->
    Fun = fun
        (Batch_task, Map) ->
            #batch_task{
                task_id = Task_id, 
                operation_type = Operation_type, 
                operation_argv = Operation_argv, 
                condition = Condition
            } = Batch_task,
            case analyze_util:get_meter_field_list_by_condition(Condition, Meter_field_list) of
                [] -> Map;
                New_meter_field_list ->
                    Fun_1 = 
                        fun(Meter_field, Map_2) ->
                                {Meter_type, Meter} = analyze_meter_field:get_meter_type_and_meter_by_meter_field(Meter_field),
                                Gateway = analyze_meter_field:get_gateway_by_meter_field(Meter_field),
                                Task_obj = #task_obj{
                                              task_id = Task_id,
                                              meter = Meter,
                                              meter_type = Meter_type, 
                                              operation_type = Operation_type, 
                                              operation_argv = Operation_argv
                                             },
                                update_map(Gateway, Task_obj, Map_2)
                        end,
                    NewMap = lists:foldl(Fun_1, Map, New_meter_field_list),
                    NewMap
            end
    end,
    Gateway_task_obj_map = lists:foldl(Fun, maps:new(), Batch_task_list),
    Gateway_task_obj_map.

update_map(Key, Element, Map) ->
    case maps:is_key(Key, Map) of
        false -> 
            Value = [Element],
            maps:put(Key, Value, Map);
        true ->
            Old_value = maps:get(Key, Map),
            Value = [Element | Old_value],
            maps:put(Key, Value, Map)
    end.

send_to_gateway(Gateway_task_obj_map) ->
    Fun = 
        fun(Gateway, Task_obj, AccIn) ->
                ?PRINT("Task_obj:~p~n", [Task_obj]),
                case analyze_gateway_pid:lookup(Gateway) of
                    {ok, Pid} ->
                        case erlang:is_process_alive(Pid) of
                            true ->
                                Pid ! {batch_task, term_to_binary(Task_obj)};
                            false ->
                                ?ERROR("gateway:~p process:~p is not alive~n", [Gateway, Pid])
                        end,
                        ok;
                    {error, _} ->
                        ?ERROR("not found pid of gateway:~p~n", [Gateway]),
                        ok
                end
        end,
    maps:fold(Fun, [], Gateway_task_obj_map).


-module(analyze_batch_task_server).

-behaviour(gen_server).

-include("analyze.hrl").
-include("analyze_config.hrl").
-include("batch_task.hrl").
-include("print.hrl").

%% Public API
-export([start_link/0]).

-export([add_batch_task/1, del_batch_task/1]).

%% gen_server callback API
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
]).

-define(SERVER, ?MODULE).

-define(INIT_STATUS, 0).
-define(RUNNING_STATUS, 1).
-define(FINISH_STATUS, 2).

-define(TIMER_INTERVAL, 1 * 60 * 1000).
-define(TIMER_MSG, check_batch_task).

-record(state, {
        batch_task_timer :: reference()       % the check cron task timer
    }).

%% API

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Data_field_str: Task_id#Start_datetime_str#End_datetime_str#Operation_type#Operation_argv#Condition
%% 000000000001#2016-09-28 13:50:00#2016-09-28 20:50:00#hz#1111#building=*&meter=150721023750&eqpt_type=*
add_batch_task(Data_field_str) ->
    case string:tokens(Data_field_str, "#") of
        [Task_id, Datetime_start_str, Datetime_end_str, Operation_type, Operation_argv, Condition] ->
            Datetime_start = get_datetime(Datetime_start_str),
            Datetime_end = get_datetime(Datetime_end_str),
            Batch_task = #batch_task{
                task_id = Task_id, 
                datetime_start = Datetime_start, 
                datetime_end = Datetime_end, 
                operation_type = Operation_type, 
                operation_argv = Operation_argv, 
                condition = Condition
            },
            add_batch_task_(Batch_task);
        _ ->
            ?ERROR("add_batch_task(~p) is not match~n", [Data_field_str])
    end,
    ok.

get_datetime(Datetime_str) ->
    %% "2016-09-27 11:16:23" Datetime_str
    [Date_str, Time_str] = string:tokens(Datetime_str, ?FS),
    Date = ?HELP:strToDate(Date_str),
    Time = ?HELP:strToTime(Time_str),
    {Date, Time}.

del_batch_task(Task_id) ->
    del_batch_task_(Task_id),
    ok.

add_batch_task_(Batch_task) ->
    gen_server:cast(?SERVER, {add_batch_task, Batch_task}).

del_batch_task_(Task_id) ->
    gen_server:cast(?SERVER, {del_batch_task, Task_id}).

%% Gen_server callbacks

init([]) ->
    State = #state{},
    {ok, State, 0}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast({add_batch_task, Batch_task}, State) ->
    do_add_batch_task(Batch_task),
    {noreply, State};
handle_cast({del_batch_task, Task_id}, State) ->
    do_del_batch_task(Task_id),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, State) ->
    analyze_batch_task_store:load_task(),
    NewState = State#state{batch_task_timer = check_batch_task_timer()},
    {noreply, NewState};

handle_info({timeout, TimerRef, ?TIMER_MSG}, State) ->
    erlang:cancel_timer(TimerRef),
    NewState = State#state{batch_task_timer = check_batch_task_timer()},
    case get_need_run_list_of_batch_task() of
        [] -> ok;
        Need_run_task_list ->
            ok = analyze_batch_task_scheduler:run_batch_task_list(Need_run_task_list),
            Fun = fun
                (Batch_task) ->
                    Batch_task#batch_task{task_status = ?RUNNING_STATUS}
            end,
            New_batch_task_list = lists:map(Fun, Need_run_task_list),
            update_task_list(New_batch_task_list),
            ok
    end,
    {noreply, NewState};
    
handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> 
    analyze_batch_task_store:close_table(),
    ok.  

code_change(_OldVersion, State, _Extra) -> {ok, State}. 

%% private functions 

%% start the batch tasks timer
check_batch_task_timer() ->
    Interval = ?TIMER_INTERVAL,
    erlang:start_timer(Interval, self(), ?TIMER_MSG).

do_add_batch_task(Batch_task) ->
    #batch_task{
        task_id = Task_id, 
        datetime_start = Datetime_start, 
        datetime_end = Datetime_end   
    } = Batch_task,
    case {is_valid_task_id(Task_id), is_valid_of_datetime(Datetime_start, Datetime_end)} of
        {true, true} ->
            New_batch_task = Batch_task#batch_task{task_status = ?INIT_STATUS},
            insert_task(New_batch_task),
            ?PRINT("do_add_batch_task is ok~n", []),
            ok;
        {true, false} ->
            ?ERROR("is_valid_of_datetime(~p, ~p) return false.~n", [Datetime_start, Datetime_end]);
        {false, true} ->
            ?ERROR("is_valid_task_id(~p) return false.~n", [Task_id]);
        {false, false} ->
            ?ERROR("is_valid_task_id(~p) and is_valid_of_datetime(~p, ~p) return false.~n", [Task_id, Datetime_start, Datetime_end])
    end,
    ok.

do_del_batch_task(Task_id) ->
    case delete_task(Task_id) of
        ok ->
            ?PRINT("~p:~p delete_task:~p is ok~n", [Task_id]);
        {error, Reason} ->
            ?ERROR("~p:~p delete_task:~p is error: ~p ~n", [Task_id, Reason])
    end,
    ok.

is_valid_task_id(Task_id) ->
    case analyze_batch_task_store:lookup_task(Task_id) of
        {error, not_found} ->
            true;
        {ok, _Task} ->
            false
    end.

is_valid_of_datetime(Datetime_start, Datetime_end) ->
    Datetime_now = ?HELP:datetime_now(),
    (Datetime_now =< Datetime_start) andalso (Datetime_start =< Datetime_end).

get_need_run_list_of_batch_task() ->
    Datetime_now = ?HELP:datetime_now(),
    Fun = fun
        (Batch_task) ->
            #batch_task{
                task_status = Task_status, 
                datetime_start = Datetime_start, 
                datetime_end = Datetime_end   
            } = Batch_task,
            case (Task_status =:= ?INIT_STATUS) andalso (Datetime_start =< Datetime_now) andalso (Datetime_now =< Datetime_end) of
                true ->
                    {true, Batch_task};
                false ->
                    false
            end
    end,
    lists:filtermap(Fun, analyze_batch_task_store:get_task_list()).


notify(Msg) ->
    ?PRINT("~p~n", [Msg]),

    ok.

insert_task(Batch_task) ->
    analyze_batch_task_store:insert_task(Batch_task).

update_task_list(Batch_task_list) ->
    analyze_batch_task_store:insert_task(Batch_task_list).

delete_task(Task_id) ->
    analyze_batch_task_store:delete_task(Task_id).



 

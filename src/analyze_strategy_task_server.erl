-module(analyze_strategy_task_server).

-behaviour(gen_server).

-include("print.hrl").
-include("cmd_obj.hrl").

-export([start_link/0]).
-export([
         add_strategy_task/2,
         delete_strategy_task/1,
         touch_strategy_task/1
        ]).

-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-define(CMD_TYPE, "dgkz").
-define(CMD_ID, "dk").

-define(SERVER, ?MODULE).

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add_strategy_task(Task_id, Gateway_cmd_list) ->
    gen_server:cast(?SERVER, {add_strategy_task, {Task_id, Gateway_cmd_list}}).

delete_strategy_task(Task_id) ->
    gen_server:cast(?SERVER, {delete_strategy_task, Task_id}).

touch_strategy_task(Task_id) ->
    gen_server:cast(?SERVER, {touch_strategy_task, Task_id}).

init([]) ->
    State = #state{},
    {ok, State, 0}.
         
handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast({add_strategy_task, {Task_id, Gateway_cmd_list}}, State) ->
    add_strategy_task_(Task_id, Gateway_cmd_list),
    {noreply, State, hibernate};
handle_cast({delete_strategy_task, Task_id}, State) ->
    delete_strategy_task_(Task_id),
    {noreply, State, hibernate};
handle_cast({touch_strategy_task, Task_id}, State) ->
    touch_strategy_task_(Task_id),
    {noreply, State, hibernate};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(timeout, State) ->
    load_strategy_task_store(),
    {noreply, State, hibernate};
handle_info({strategy_task_result, Result}, State) ->
    ?PRINT("strategy_task_result:~p~n", [Result]),
    {noreply, State, hibernate};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    stop_strategy_task_store(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

add_strategy_task_(Task_id, Gateway_cmd_list) ->
    analyze_strategy_task_store:insert(Task_id, Gateway_cmd_list).

delete_strategy_task_(Task_id) ->
    analyze_strategy_task_store:delete(Task_id).

touch_strategy_task_(Task_id) ->
    case analyze_strategy_task_store:lookup(Task_id) of
        {error, Reason} ->
            ?ERROR("lookup strategy task_id:~p is: ~p~n", [Task_id, Reason]);
        {ok, Gateway_cmd_list} ->
            lists:foreach(fun send_to_gateway/1, Gateway_cmd_list)
    end.

send_to_gateway(Gateway_cmd = {Gateway, Gateway_type, Cmd_data}) ->
    case analyze_gateway_pid:lookup(Gateway) of
        {ok, Pid} ->
            case erlang:is_process_alive(Pid) of
                true ->
                    Gateway_cmd_obj = gen_gateway_cmd_obj(Gateway, Gateway_type, Cmd_data),
                    Pid ! {strategy_task, self(), Gateway_cmd_obj};
                false ->
                    ?ERROR("gateway: ~p process is not alive:~p ~n", [Gateway, Gateway_cmd])
            end;
        {error, Reason} ->
            ?ERROR("not found pid of gateway:~p~n", [Gateway_cmd])
    end.
    
load_strategy_task_store() ->
    analyze_strategy_task_store:load().

stop_strategy_task_store() ->
    analyze_strategy_task_store:close().

gen_gateway_cmd_obj(Gateway, Gateway_type, Cmd_data) ->
    #cmd_obj{
       eqpt_type = Gateway_type,
       eqpt_id_code = Gateway,
       cmd_type = ?CMD_TYPE,
       cmd_id = ?CMD_ID,
       cmd_data = Cmd_data
      }.


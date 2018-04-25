-module(analyze_server).

-behaviour(gen_server).

-include("analyze.hrl").
-include("print.hrl").
-include("time_table.hrl").

-define(SERVER, ?MODULE).

-export([start_link/0]).

-export([add_meters/1, del_meters/1, batch_update_build/1, delete_meters_by_gateway/1]).

-export([add_time_table/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).


-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add_meters(MeterFieldList) ->
    gen_server:cast(?SERVER, {add_meters, MeterFieldList}).

del_meters(MeterTypeAndMeterList) ->
    gen_server:cast(?SERVER, {del_meters, MeterTypeAndMeterList}).

batch_update_build(MeterTypeAndMeterAndBuildIdList) ->
    gen_server:cast(?SERVER, {batch_update_build, MeterTypeAndMeterAndBuildIdList}).

delete_meters_by_gateway(Gateway) ->
    gen_server:cast(?SERVER, {delete_meters_by_gateway, Gateway}).

add_time_table(Time_table_field) ->
    gen_server:cast(?SERVER, {add_time_table, Time_table_field}).

init(_Args) ->
    State = #state{},
    {ok, State}.
    
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({add_meters, MeterFieldList}, State) -> 
    do_add_meters(MeterFieldList),
    {noreply, State};

handle_cast({del_meters, MeterTypeAndMeterList}, State) ->
    do_del_meters(MeterTypeAndMeterList),
    {noreply, State};

handle_cast({batch_update_build, MeterTypeAndMeterAndBuildIdList}, State) ->
    do_batch_update_build(MeterTypeAndMeterAndBuildIdList),
    {noreply, State};

handle_cast({delete_meters_by_gateway, Gateway}, State) ->
    do_delete_meters_by_gateway(Gateway),
    {noreply, State};

handle_cast({add_time_table, Time_table_field}, State) ->
    do_add_time_table(Time_table_field),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    ?ERROR("~p~n", [Info]),
    {noreply, State}.

terminate(Reason, State) ->
    ?ERROR("~p~n", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% Internal functions

%% 添加新的表监控
do_add_meters(MeterFieldList) ->
    Fun = 
        fun(Meter_field) ->
                {Meter_type, Meter} = analyze_meter_field:get_meter_type_and_meter_by_meter_field(Meter_field),    
                case analyze_meter_field_store:lookup(Meter_type, Meter) of
                    {ok, Meter_field} -> %% 此处特地使用bound的Meter_field
                        %% Meter_field 未修改到，无需任何操作
                        ok;
                    _ ->
                        Build_id = analyze_meter_field:get_build_id_by_meter_field(Meter_field),
                        Gateway = analyze_meter_field:get_gateway_by_meter_field(Meter_field),
                        handle_add_build_process(Build_id),
                        handle_add_gateway_process(Gateway),
                        analyze_meter_sup:stop_meter(Meter_type, Meter),
                        analyze_meter_sup:start_meter(Meter_field),
                        ok
                end
        end,
    lists:foreach(Fun, MeterFieldList),
    analyze_meter_field_server:insert_meter_field(MeterFieldList),
    ok.

%% 删除表监控
do_del_meters(MeterTypeAndMeterList) ->
    Fun = 
        fun({Meter_type, Meter}) ->
                case analyze_meter_field_store:lookup(Meter_type, Meter) of
                    {ok, Meter_field} ->
                        Build_id = analyze_meter_field:get_build_id_by_meter_field(Meter_field),
                        Gateway = analyze_meter_field:get_gateway_by_meter_field(Meter_field),
                        handle_del_build_process(Build_id, Meter_type, Meter),
                        handle_del_gateway_process(Gateway, Meter_type, Meter),
                        analyze_meter_field_server:delete_meter_field({Meter_type, Meter}),
                        analyze_meter_sup:stop_meter(Meter_type, Meter),
                        ok;
                    {error, _Reason} ->
                        ok
                end
        end,
    lists:foreach(Fun, MeterTypeAndMeterList),
    ok.
    
do_batch_update_build(MeterTypeAndMeterAndBuildIdList) ->
    Fun = 
        fun({Meter_type, Meter, Build_id}, AccIn) ->
                case analyze_meter_field_store:lookup(Meter_type, Meter) of
                    {ok, Meter_field} ->                        
                        handle_del_build_process(Build_id, Meter_type, Meter),
                        handle_add_build_process(Build_id),
                        [analyze_meter_field:update_build_id(Meter_field, Build_id) | AccIn];
                    _ ->
                        AccIn
                end
        end,
    MeterFieldList = lists:foldl(Fun, [], MeterTypeAndMeterAndBuildIdList),
    analyze_meter_field_server:insert_meter_field(MeterFieldList),
    ok.                         

do_delete_meters_by_gateway(Gateway) ->
    case analyze_gateway_meter:lookup(Gateway) of 
        {ok, MeterTypeAndMeterList} ->
            do_del_meters(MeterTypeAndMeterList);
        _ ->
            ok
    end,
    analyze_gateway_sup:stop_gateway(Gateway).            

%% 处理建筑进程
handle_add_build_process(Build_id) ->
    analyze_build_sup:start_build_room(Build_id).

handle_del_build_process(Build_id, Meter_type, Meter) ->
    case analyze_build_meter:lookup(Build_id) of
        {ok, [{Meter_type, Meter}]} ->
            analyze_build_sup:stop_build(Build_id);
        _ ->
            ok
    end.

%% 检查是否存在build进程
is_exist_build_process(Build_id) ->
    case analyze_build_pid:lookup(Build_id) of
        {ok, _Pid} ->
            true;
        {error, Reason} ->
            false
    end.

%% 处理网关进程
handle_add_gateway_process(Gateway) ->
    analyze_gateway_sup:start_gateway(Gateway).

handle_del_gateway_process(Gateway, Meter_type, Meter) ->
    case analyze_gateway_meter:lookup(Gateway) of
        {ok, [{Meter_type, Meter}]} ->
            analyze_gateway_sup:stop_gateway(Gateway);
        _ ->
            ok
    end.
    

%% 检查是否存在gateway进程
is_exist_gateway_process(Gateway) ->
    case analyze_gateway_pid:lookup(Gateway) of
        {ok, _Pid} ->
            true;
        {error, Reason} ->
            false
    end.

spawn_meter(Meter_field) ->
    {Meter_type, Meter} = analyze_meter_field:get_meter_type_and_meter_by_meter_field(Meter_field),
    stop_meter(Meter_type, Meter),
    case is_exist_module(Meter_type) of
        {ok, Child_module} ->
            {ok, Pid} = Child_module:start_link(Meter_field),
            insert_meter_process(Meter_type, Meter, Pid),
            {ok, Pid};
        {error, Reason} -> 
            {error, Reason}
    end.

stop_meter(Meter_type, Meter) ->
    case analyze_meter_util:get_running_pid(Meter_type, Meter) of
        {ok, Pid} ->
            exit(Pid, shutdown),
            delete_meter_process({Meter_type, Meter});
        {error, _What} ->
            ok
    end.

%% 是否存在对应表类型的虚拟设备模块
is_exist_module(Meter_type) ->
    case get_module_name_of_type(Meter_type) of
        {ok, Child_module} ->
            case is_loaded(Child_module) of
                true          -> {ok, Child_module};
                {error, What} -> {error, What}
            end;
        {error, Reason} -> {error, Reason}       
    end.
    
%% 根据表类型获取对应的模块名
get_module_name_of_type(Meter_type) ->
    case lists:keyfind(Meter_type, 1, ?METER_TYPE_TO_MODULE)  of
        {Meter_type, Module_name} -> {ok, Module_name};
        false -> {error, not_found_module}
    end.

%% 模块是否加载
is_loaded(Module) ->
    case code:is_loaded(Module) of
        false ->
            case code:load_file(Module) of
                {module, Module} -> true;
                {error, What} -> {error, What}
            end;
        {file, _} ->
            true
    end.

%% 建立表与虚拟表进程的关系
insert_meter_process(Meter_type, Meter, Pid) ->
    analyze_meter_pid:insert(Meter_type, Meter, Pid).

%% 删除表与虚拟表进程的关系
delete_meter_process({Meter_type, Meter}) ->
    analyze_meter_pid:delete({Meter_type, Meter}).

do_add_time_table(Time_table_field) ->
    %{"0001000100020002",[["0a0001aa7k","ac"],["0a0001aa8k","lighting"]],{2016,8,21},{2017,1,20},[{1,[{{8,0},{12,0}},{{13,0},{18,0}},{{18,40},{21,30}}]},{2,[{{8,0},{12,0}},{{13,0},{18,0}},{{18,40},{21,30}}]}]}
    ?ERROR("~p~n", [Time_table_field]),
    {Room_id, Type_and_labels_of_meter_list, Level, Validity_date_start, Validity_date_end, Holiday_mode, Time_list} = Time_table_field,
    Fun = fun
        (Type_and_labels_of_meter) ->
            Time_table_record = #time_table{
                key = {Room_id, Type_and_labels_of_meter, Level},
                room_id = Room_id,
                type_and_labels_of_meter = Type_and_labels_of_meter,
                level = Level,
                validity_date_start = Validity_date_start,
                validity_date_end = Validity_date_end,
                holiday_mode = Holiday_mode,
                time_list = Time_list
            }
    end,
    Time_table_record_list = lists:map(Fun, Type_and_labels_of_meter_list),
    analyze_build_time_table_store:insert(Time_table_record_list),
    ok.


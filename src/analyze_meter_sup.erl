-module(analyze_meter_sup).

-behaviour(supervisor).

-include("analyze.hrl").
-include("analyze_config.hrl").
-include("analyze_meter.hrl").
-include("print.hrl").

-export([start_link/0]).

-export([start_meter/1, stop_meter/2]).

-export([init/1]).

-define(SERVER, ?MODULE).

%%============================================================================================
%% API
%%============================================================================================

start_link() ->
    {ok, Pid} = supervisor:start_link({local, ?SERVER}, ?MODULE, []),
    create_processes(),
    {ok, Pid}.

%%============================================================================================
%% API end
%%============================================================================================-

init(_Args) ->    
    %% 初始化各种存储关系
    init_relationship(),

    %% 构建触发式任务
    Tasks_config_file = ?TASKS_CONFIG,
    build_datamsg_tasks(Tasks_config_file),
    
    SupFlags = #{strategy => simple_one_for_one, intensity => 5, preiod => 10},
    ChildSpecs = [#{id => analyze_meter, start => {analyze_meter, start_link, []},
                    restart => transient, shutdown => 5000, type => worker, modules => 'dynamic'
                   }],
    {ok, {SupFlags, ChildSpecs}}.

%% -------------------------------------------------
%% Internal functions
%% -------------------------------------------------

%% 初始化虚拟表与相关进程的对应关系
init_relationship() ->
    %% 加载卡座活动流数据
    analyze_realtime_workflow:load_workflow(),
    %% 加载分体空调工作活动存储
    analyze_meter_work_activities_store:load_activities(),
    %% 表与进程id对应关系 
    analyze_meter_pid:init(),
    %% 数据上报对应的任务
    analyze_datamsg_to_tasks:init(),
    %% 表的最近上报的数据块
    analyze_meter_to_blob:load_blob(),
    %% 表的通讯质量
    analyze_meter_to_cq:init(),
    %% 设备的开关状态
    analyze_meter_on_off_status:init(),
    ok.

start_meter(Meter_field) ->
    {Meter_type, Meter} = analyze_meter_field:get_meter_type_and_meter_by_meter_field(Meter_field),
    case analyze_meter_util:get_running_pid(Meter_type, Meter) of
        {ok, Pid} ->
            case erlang:is_process_alive(Pid) of
                true ->
                    {error, started};
                false ->
                    supervisor:start_child(?MODULE, [Meter_field])
            end;
        _NotStarted ->
            supervisor:start_child(?SERVER, [Meter_field])
    end.

stop_meter(Meter_type, Meter) ->
    case analyze_meter_util:get_running_pid(Meter_type, Meter) of
        {ok, Pid} ->
            supervisor:terminate_child(?SERVER, Pid);
        _NotStarted ->
            {error, stoped}
    end.

create_processes() ->
    Fun =
        fun(Meter_field, {Count, Start}) ->
                try ?MODULE:start_meter(Meter_field) of
                    {ok, _Pid} ->
                        {Count+1, Start+1};
                    {error, Reason} ->
                        ?ERROR("~p spawn error:~p~n", [Meter_field, Reason]), 
                        {Count+1, Start}
                catch 
                    _Class:Reason ->
                        ?ERROR("~p spawn error:~p~n", [Meter_field, Reason]),
                        {Count+1, Start}
                end
        end,
    {Count, Start} = analyze_meter_field_store:foldl(Fun, {0, 0}),
    ?PRINT("Count ~p meters is Started ~p~n", [Count, Start]),
    ok.

%% 建立触发式任务
build_datamsg_tasks(Tasks_config_file) ->
    case file:consult(Tasks_config_file) of
        {ok, Datamsg_tasks_list}->
            %% 初始化触发式任务列表
            set_datamsg_tasks(Datamsg_tasks_list);
        {error, Reason} ->
            ?ERROR("parse(~p) is error: ~p~n", [Tasks_config_file, file:format_error(Reason)]),
            exit(Reason)
    end.
    
%% 设定上报类型的任务列表
set_datamsg_tasks(Datamsg_tasks_list)->
    analyze_datamsg_to_tasks:insertAll(Datamsg_tasks_list).

%% 建立表与虚拟表进程的关系
insert_meter_process(Meter_type, Meter, Pid) ->
    analyze_meter_to_pid:insert(Meter_type, Meter, Pid).

%% 删除表与虚拟表进程的关系
delete_meter_process({Meter_type, Meter}) ->
    analyze_meter_to_pid:delete({Meter_type, Meter}).


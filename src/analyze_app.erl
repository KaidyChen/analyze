-module(analyze_app).

-behaviour(application).

-include("analyze.hrl").
-include("analyze_config.hrl").
-include("print.hrl").

%% Application callbacks
-export([
    start/2,
    stop/1
]).

%%%===================================================================
%% Application callbacks
%%%===================================================================

-spec(start(StartType, StartArgs) -> {ok, Pid} | {ok, Pid, State} | {error, Reason} when
      StartType :: normal | {takeover, Node :: node()} | {failover, Node :: node()},
      StartArgs :: term(),
      State     :: term(),
      Reason    :: term()
    ).
start(_StartType, _StartArgs) ->
    %% 输出启动信息
    print_banner(),

    %% 设置env
    set_application_env(),

    %% 启动应用的监督进程
    {ok, Sup} = analyze_sup:start_link(),
    %% 开启应用的基本服务
    start_servers(Sup),
    %% 开启socket监听服务
    start_listeners(),

    register(analyze, self()),

    print_vsn(),

    %% 回调函数返回{ok, pid()}
    {ok, Sup}.

stop(_State) ->
    catch stop_listeners(),
    ok.

%%%====================================================================
%% Print Banner API
%%%====================================================================

print_banner() ->
    ?INFO("starting ~p on node '~s'", [?MODULE, node()]).

print_vsn() ->
    %% 根据key获取*.app.src的值
    {ok, Vsn} = application:get_key(vsn),
    {ok, Desc} = application:get_key(description),
    ?INFO("~s ~s is running now", [Desc, Vsn]).

%%%====================================================================
%% End Print Banner API
%%%====================================================================

%%%====================================================================
%% Set Application Env
%%%====================================================================
  
%% Set application env
set_application_env() ->
    %% 设置节假日env
    set_holiday_env(),
    %% 设置设备上报间隔env
    set_report_interval_env(),
    ok.
  
%% Set Holiday env
set_holiday_env() ->
    {ok, HolidayList} = 
        case analyze_util:get_holiday_list() of
            {ok, List} ->
                {ok, List};
            {error, Reason} ->
                ?ERROR("get_holiday_list() is error:~p~n", [Reason]),
                {ok, []}
        end,        
    app_util:set_env(holidays, HolidayList).

set_report_interval_env() ->
    File = ?REPORT_INTERVAL_CONFIG,
    {ok, ReportIntervalList} = 
        case file:consult(File) of
            {ok, List} ->
                {ok, List};
            {error, Reason} ->
                ?ERROR("file:consult(~p) is error:~p~n", [File, Reason]),
                {ok, []}
        end,
    app_util:set_env(report_intervals, ReportIntervalList).
                    

%%%====================================================================
%% End API
%%%====================================================================


%%%====================================================================
%% Start servers
%%%====================================================================
start_servers(Sup) ->
    Servers = [
               {"analyze server", analyze_server},
               {"analyze meter field server", analyze_meter_field_server},
               {"analyze push data server", analyze_push_data_server},
               %% gen_event处理事件触发
               {"analyze event", analyze_event},
               %% analyze_crontab_server ：定时执行任务模块
               {"analyze crontab server", analyze_crontab_server},
               %% analyze_meter_sup ：虚拟设备表的监控进程
               {"analyze meter supervisor", {supervisor, analyze_meter_sup}},
               %% analyze_gateway_sup ：虚拟网关的监控进程
               {"analyze gateway supervisor", {supervisor, analyze_gateway_sup}},
               %% analyze_build_sup ：虚拟建筑的监控进程
               {"analyze build supervisor", {supervisor, analyze_build_sup}},
               %% analyze_meter_quota_server ：定额管理任务服务进程
               {"analyze meter quota", analyze_meter_quota_server},
               %% analyze_batch_task_scheduler ：批量任务的调度进程
               {"analyze batch task scheduler", analyze_batch_task_scheduler},
               %% analyze_batch_task_server ：批量任务的服务进程
               {"analyze batch task", analyze_batch_task_server},
               %% analyze_xml_report_server ：XML上报服务进程
               {"analyze xml report server", analyze_xml_report_server},
               %% analyze_strategy_task_server: 灯光策略任务服务
               {"analyze lighting stragegy task server", analyze_lighting_strategy_server}
              ],
    [start_server(Sup, Server) || Server <- Servers].

start_server(_Sup, {Name, F}) when is_function(F) ->
    ?INFO("~s is starting...", [Name]),
    F(),
    ?INFO_MSG("[done]");
start_server(Sup, {Name, Server}) ->
    ?INFO("~s is starting...", [Name]),
    {ok, _Child} = app_util:start_child(Sup, Server),
    ?INFO_MSG("[done]");

start_server(Sup, {Name, Server, Opts}) ->
    ?INFO("~s is starting...", [Name]),
    {ok, _Child} = app_util:start_child(Sup, Server, Opts),
    ?INFO_MSG("[done]").
    
%%-----------------------------------------------------------------------------
%% Start listenners
%%-----------------------------------------------------------------------------

%% @doc Start listeners
-spec(start_listeners() -> any()).
start_listeners() ->
    lists:foreach(fun start_listener/1, app_util:env(listeners, [])).

start_listener({tcp, report_data, NbAcceptors, TransOpts, ProtoOpts}) ->
    {ok, _} = ranch:start_listener(report_data, NbAcceptors,
                                    ranch_tcp, TransOpts,
                                    analyze_report_data_protocol, ProtoOpts),
    ?INFO("Listener Proto: ~p NbAcceptor: ~p TransOpts: ~p ProtoOpts: ~p~n", 
          [{tcp, report_data}, NbAcceptors, TransOpts, ProtoOpts]);
start_listener({tcp, master_control, NbAcceptors, TransOpts, ProtoOpts}) ->
    {ok, _} = ranch:start_listener(master_control, NbAcceptors,
                                    ranch_tcp, TransOpts,
                                    analyze_master_control_protocol, ProtoOpts),
    ?INFO("Listener Proto: ~p NbAcceptor: ~p TransOpts: ~p ProtoOpts: ~p~n", 
          [{tcp, master_control}, NbAcceptors, TransOpts, ProtoOpts]);
start_listener({tcp, rpc, NbAcceptors, TransOpts, ProtoOpts}) ->
    {ok, _} = ranch:start_listener(rpc, NbAcceptors,
                                    ranch_tcp, TransOpts,
                                    analyze_rpc_protocol, ProtoOpts),
    ?INFO("Listener Proto: ~p NbAcceptor: ~p TransOpts: ~p ProtoOpts: ~p~n", 
          [{tcp, rpc}, NbAcceptors, TransOpts, ProtoOpts]);
start_listener({http, client_request, NbAcceptors, TransOpts, ProtoOpts}) ->
    Dispatch = dispatch_rules(),
    ProtoOptsTmp = #{
      env => #{dispatch => Dispatch}
     },
    {ok, _} = cowboy:start_clear(client_request, NbAcceptors, TransOpts,
                                 maps:merge(ProtoOptsTmp, maps:from_list(ProtoOpts))),
    ?INFO("Listener Proto: ~p NbAcceptor: ~p TransOpts: ~p ProtoOpts: ~p~n", [{http, client_request},  NbAcceptors, TransOpts, ProtoOpts]).

dispatch_rules() ->
    cowboy_router:compile([
                           {
                             '_', [
                                   {"/[...]", analyze_rest_handler, []}
                                  ]
                           }
                          ]).
        
%%-----------------------------------------------------------------------------
%% Stop listerners
%%-----------------------------------------------------------------------------

%% @doc Stop listeners
stop_listeners() ->
    lists:foreach(fun stop_listener/1, app_util:env(listeners, [])).

stop_listener({tcp, Ref, _, _, _}) ->
    ranch:stop_listener(Ref);
stop_listener({http, Ref, _, _, _}) ->
    cowboy:stop_listener(Ref).







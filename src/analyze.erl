%%%=========================================================================================
%% 数据分析应用
%% 1，大并发线程，对所有接入的能源计量设备来虚拟化计算 
%% 2，每个虚拟设备，可挂载指标计算任务，和主控任务。 
%% 3，指标计算任务，如按时间的统计计算，相应结果可通过命令调用，利于移动端app显示。 
%% 4，主控任务，如数据完备性任务，通讯质量任务，以及一些应用型任务（空调通宵开机判断）
%%%=========================================================================================

-module(analyze).

%% eunit
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% Include *.hrl
-include("print.hrl").

%% export functions
-export([
    start/0, 
    stop/0
]).

%% define 
-define(APP, ?MODULE).

%% 应用启动顺序列表：
%% 1. lager:日志
%% 2. crontab:定时执行
%% 3. ?APP:本项目应用名
-define(APPS_LIST, [lager, crontab, ?APP]).

%%%==========================================================================================
%% Bootstrap, stop
%%%==========================================================================================

%% @doc Start application
-spec(start() -> ok | {error, any()}).
start() ->
    %% 启动应用列表
    [ok = app_util:start_app(App) || App <- ?APPS_LIST],
    ok.

%% @doc Stop application
stop() ->
    %% 停止应用列表
    [app_util:stop_app(App) || App <- ?APPS_LIST],
    ok.


-ifdef(TEST).

start_test_() ->
    start(),
    [?assertEqual(ok, application:ensure_started(lager)),
     ?assert(ok =:= application:ensure_started(crontab)),
     ?assertNotMatch({error, _}, application:ensure_started(?APP))
    ].

-endif.





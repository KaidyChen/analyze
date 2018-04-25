-module(analyze_event).

-include("analyze.hrl").
-include("analyze_config.hrl").

%% public API
-export([start_link/0]).
-export([add_handler/2,
         delete_handler/2
]).

%% notify event API
-export([report_data/3,
         add_meter/1,
         del_meter/1
]).

-define(SERVER, ?MODULE).

%%======================================================================
%% Public API
%%======================================================================

start_link() ->
    {ok, Pid} = gen_event:start_link({local, ?SERVER}),
    %% 添加日志事件处理,需要在analyze_event开启之后调用
    analyze_log_event:add_handler(),
    {ok, Pid}.

%% @doc add a event handler to server
add_handler(Handler, Args) ->
    gen_event:add_sup_handler(?SERVER, Handler, Args).

%% @doc remove a event handler to server
delete_handler(Handler, Args) ->
    gen_event:delete_handler(?SERVER, Handler, Args).

%%======================================================================
%% End Public API
%%======================================================================


%%======================================================================
%% Notify event API
%%======================================================================

%% @doc receive MsgType is "dataMsg" or "warnMsg" notify a event
report_data(Msg_type, Data_list, Now_datetime) ->
    gen_event:notify(?SERVER, {report_data, Msg_type, Data_list, Now_datetime}).

%% @doc receive MsgType is "addMeter" notify a event
add_meter(Msg) ->
    gen_event:notify(?SERVER, {add_meter, Msg}).
  
%% @doc receive MsgType is "delMeter" notify a event  
del_meter(Msg) ->
    gen_event:notify(?SERVER, {del_meter, Msg}).
    
%%======================================================================
%% End Notify event API
%%======================================================================




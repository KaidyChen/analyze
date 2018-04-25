-module(analyze_report_data_protocol).

-behaviour(gen_server).

-include("print.hrl").
-include("analyze.hrl").
-include("analyze_config.hrl").
-include("report.hrl").

-export([start_link/4]).

-export([init/1, 
         init/4,
         handle_call/3, 
         handle_cast/2, 
         handle_info/2,
         terminate/2, 
         code_change/3
]).

-define(TIMEOUT, 5 * 1000).

-record(state, {
          socket,
          transport
         }).

start_link(Ref, Socket, Transport, Opts) ->
    proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).

init([]) -> {ok, undefined}.

init(Ref, Socket, Transport, Opts) ->
    ok = proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{active, once}]),
    State = #state{
               socket = Socket,
               transport = Transport
              },
    gen_server:enter_loop(?MODULE, [], State, ?TIMEOUT).

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, #state{socket = Socket, transport = Transport} = State) ->
    Transport:close(Socket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info({tcp, Socket, Data_Bin}, #state{socket = Socket, transport = Transport} = State) ->
    ok = Transport:setopts(Socket, [{active, once}]),
    Data_list = string:strip(string:strip(binary_to_list(Data_Bin), right, $\n),right, $\r),
    Now_datetime = ?HELP:datetime_now(),
    %% 电能数据块上报: dataMsg/0a0001aa7k#002014110119#3832ffef5a94353363565b3333663333c83532ca7c4a383333373783363b3949
    %% 功率超限上报:   warnMsg/0a0001aa7k#002014110119#3532ffef***************************************************
    case string:tokens(Data_list, "/") of
        [Msg_type, Data_field_str] when (Msg_type =:= ?DATAMSG) orelse (Msg_type =:= ?STATUSMSG) 
                                        orelse (Msg_type =:= ?WARNMSG) 
                                        orelse (Msg_type =:= ?CONTROLMSG) ->
            analyze_event:report_data(Msg_type, Data_list, Now_datetime),
            analyze_report_data:report_data(Msg_type, Data_field_str, Now_datetime),
            ok;
        _ ->
            ?ERROR("Report_data: ~p is not match~n", [Data_list])
    end,
    %% 立即触发超时
    {noreply, State, 0};
handle_info({tcp_closed, _Socket}, #state{socket = _Socket} = State) ->
    {stop, normal, State};
handle_info({tcp_error, _, Reason}, #state{socket = _Socket} = State) ->
    {stop, Reason, State};
handle_info(timeout, #state{socket = Socket} = State) ->
    %% 超时结束进程
    {stop, normal, State};
handle_info(_Info, State) ->
    {stop, normal, State}.


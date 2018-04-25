-module (analyze_push_service).

-include("analyze.hrl").
-include("analyze_config.hrl").
-include("print.hrl").

-compile([export_all]).

-export([push/2]).

push(Msg_type, Msg_body) ->
    Msg = get_msg(Msg_type, Msg_body),
    push_0(Msg).

push_0(Msg) ->
    ?PRINT("~p~n", [Msg]),
    {Ip, Port} = get_push_service_ip_and_port(),
    push_1(Ip, Port, Msg),
    ok.

push_1(Ip, Port, Msg) ->
    case gen_tcp:connect(Ip, Port, ?CONN_OPTS, ?SOCKET_TIMEOUT) of
        {ok, Socket} ->
            try gen_tcp:send(Socket, Msg) of
                ok ->
                    ?PRINT("Send Msg:~s~n", [Msg]),
                    ok;
                {error, What} ->
                    ?ERROR("gen_tcp:send ~p is error:~p~n", [Msg, What])
            catch
                Class:Ex ->
                    ?ERROR("gen_tcp:send ~p is catch:~p:~p~n",  [Msg, Class, Ex])
            after
                gen_tcp:close(Socket)
            end;
        {error, Reason} ->
            ?ERROR("gen_tcp:connect ~p:~p is error:~p~n", [Ip, Port, Reason])
    end.
    
get_msg(Msg_type, Msg_body) ->
    Msg = string:join([Msg_type, Msg_body], "/"),
    ?HELPER:to_iolist(Msg).

get_push_msg(Meter_type, Meter, Msg_type, Msg_body) ->
    Chinese_meter_type = analyze_util:get_chinese_meter_type(Meter_type),
    Msg = string:join([Msg_type, Chinese_meter_type, Meter, Msg_body], "#"),
    string:join([Meter_type, Meter, Msg], "/").

get_push_service_ip_and_port() ->
    {ok, PushServiceOpt} = app_util:env(push_service),
    proplists:get_value(connect, PushServiceOpt).


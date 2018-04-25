-module(analyze_push_data_server).

-behaviour(gen_server).

-include("analyze.hrl").
-include("analyze_config.hrl").
-include("print.hrl").

%-compile([export_all]).

-export([start_link/0]).
-export([push_hour_data/3]).
-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-define(SERVER, ?MODULE).

-define(TIMER_INTERVAL, 1*20*1000).
-define(TIMER_MSG, timer_msg).
-define(INIT_STATE, begin 
                        #state{
                           list = [], 
                           listLen = 0, 
                           socket = undefined,
                           timer = erlang:start_timer(?TIMER_INTERVAL, self(), ?TIMER_MSG)
                          } 
                    end).

-record(state, {
          list :: list(),
          listLen :: pos_integer(),
          socket :: port(),
          timer :: reference()
         }).

-define(LEN, 100).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

push_hour_data(Meter, Datetime, Electric_power) ->
    case app_util:env_proplists(hour_data_push, enable) of
        true ->
            gen_server:cast(?SERVER, {hour_data, {Meter, Datetime, Electric_power}});
        _ ->
            ok
    end.

init([]) ->
    State = #state{
               list = [],
               listLen = 0,
               socket = undefined,
               timer = erlang:start_timer(?TIMER_INTERVAL, self(), ?TIMER_MSG)
              },
    {ok, State, hibernate}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast({hour_data, {Meter, Datetime, Electric_power} = Item}, State) ->
    ?PRINT("Item: ~p~n", [Item]),
    #state{
       list = List,
       listLen = ListLen,
       socket = Socket
      } = State,
    NewState = 
        case ListLen < ?LEN of
            true -> 
                State#state{
                  list = [Item | List],
                  listLen = ListLen+1
                 };
            false ->
                Packet = gen_packet(List),
                State#state{
                  list = [],
                  listLen = 0,
                  socket = send(Packet, Socket)
                 }
        end,
    {noreply, NewState};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({timeout, TimerRef, ?TIMER_MSG}, State = #state{timer = Timer, list = List, socket = Socket}) 
  when (TimerRef =:= Timer) ->
    cancel_timer(TimerRef),
    NewState = 
        case List of
            [] -> 
                State#state{
                  list = [],
                  listLen = 0,
                  timer = erlang:start_timer(?TIMER_INTERVAL, self(), ?TIMER_MSG)
                 };
            _ -> 
                Packet = gen_packet(List),
                State#state{
                  list = [],
                  listLen = 0,
                  socket = send(Packet, Socket),
                  timer = erlang:start_timer(?TIMER_INTERVAL, self(), ?TIMER_MSG)
                 }
        end,
    {noreply, NewState};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

cancel_timer(TimerRef) when is_reference(TimerRef) ->
    erlang:cancel_timer(TimerRef, [{async, true}, {info, false}]);
cancel_timer(_) ->
    ok.

gen_packet(List) ->
    Fun =
        fun({Meter, Datetime, Electric_power}) when is_list(Electric_power) ->
                "|" ++ string:join([Meter, ?HELP:getDateTimeStr(Datetime), Electric_power], ",");
           ({Meter, Datetime, Electric_power}) ->     
                "|" ++ string:join([Meter, ?HELP:getDateTimeStr(Datetime), ?HELP:float_to_decimal_str(Electric_power, 2)], ",")
        end,
    string:join(lists:map(Fun, List), "").

send(Packet, Socket) when (Socket =:= undefined) ->
    case get_socket() of
        {ok, NewSocket} ->
            send(Packet, NewSocket);
        {error, Reason} ->
            ?PRINT("get_socket: ~p~n", [Reason]),
            undefined
    end;
send(Packet, Socket) when is_port(Socket) ->
    case gen_tcp:send(Socket, Packet) of
        ok -> 
            ?PRINT("Packet: ~p~n", [Packet]),
            Socket;
        {error, Reason} ->
            ?PRINT("send :~p~n", [Reason]),
            gen_tcp:close(Socket),
            send(Packet, undefined)
    end.

get_socket() ->
    {Ip, Port} = get_hour_data_push_ip_and_port(),
    case gen_tcp:connect(Ip, Port, ?CONN_OPTS, ?SOCKET_TIMEOUT) of
        {ok, Socket} ->
            {ok, Socket};
        {error, Reason} ->
            {error, Reason}
    end.

get_hour_data_push_ip_and_port() ->
    {ok, HourDataPushOpt} = app_util:env(hour_data_push),
    proplists:get_value(connect, HourDataPushOpt).

-module(analyze_rpc_protocol).

-include("print.hrl").
-include("analyze_config.hrl").

-behaviour(gen_server).

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

-define(RPC_API, rpc_api).

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
    ok = Transport:setopts(Socket, [{active, once}, {send_timeout, ?SOCKET_TIMEOUT}]),
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

handle_info({tcp, Socket, RawData}, #state{socket = Socket, transport = Transport} = State) ->
    ok = Transport:setopts(Socket, [{active, once}]),
    ?PRINT("API RawData:~p~n", [RawData]),
    do_rpc(Socket, RawData),
    {noreply, State, 0};
handle_info({tcp_closed, _Socket}, #state{socket=_Socket}=State) ->
    {stop, normal, State};
handle_info({tcp_error, _, Reason}, #state{socket=_Socket}=State) ->
    {stop, Reason, State};
handle_info(timeout, #state{socket=_Socket}=State) ->
    {stop, normal, State};
handle_info(_Info, State) ->
    {stop, normal, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

do_rpc(Socket, RawData) ->
    try
        {F, A} = split_out_fa(RawData),
        ?PRINT("F:~p A:~p~n", [F, A]),
        Result = apply(?RPC_API, F, A),
        ?PRINT("~p~n", [Result]),
        gen_tcp:send(Socket, Result)
    catch
        _Class:Err ->
            ?ERROR("do_rpc RawData:~p is error:~p", [RawData, Err]),
            gen_tcp:send(Socket, <<"error">>)
    end,
    ok.                                    

split_out_fa(RawData) ->
    FA = re:replace(RawData, "\r\n$", "", [{return, list}]),
    ?PRINT("~p~n", [FA]),
    {match, [F, A]} =
        re:run(FA,
               "(.*)\s*\\((.*)\s*\\)\s*\s*$",
                   [{capture, [1,2], list}, ungreedy]),
    {list_to_atom(F), args_to_terms(A)}.

args_to_terms(RawArgs) ->
    {ok, Toks, _Line} = erl_scan:string("[" ++ RawArgs ++ "]. ", 1),
    {ok, Args} = erl_parse:parse_term(Toks),
    Args.

-module(analyze_server_socket).

-behaviour(gen_server).

-include("print.hrl").

%% Public API
-export([start_link/1]).

-export([accept_loop/1]).  

%% gen_server callback API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(DEFAULT_OPT, [binary, {packet, raw}, {active, once}, {reuseaddr, true}]).

-record(socket_state, {port, listenOpt, listenSocket, protocol}).

%%========================================================================================
%% API
%%========================================================================================

%% @doc start listeners
start_link([Port, Protocol]) when is_atom(Protocol) ->
    start_link([Port, ?DEFAULT_OPT, Protocol]);

start_link([Port, ListenOpt, Protocol]) when is_list(ListenOpt) andalso is_atom(Protocol) ->
    State = #socket_state{port = Port, listenOpt = ListenOpt, protocol = Protocol},
    gen_server:start_link(?MODULE, State, []).

init(#socket_state{port = Port, listenOpt = ListenOpt, protocol = Protocol} = State) ->
    Reply = case gen_tcp:listen(Port, ListenOpt) of
                {ok, LSocket} ->
                    ?INFO("~p listening on port: ~p", [Protocol, Port]),
                    NewState = State#socket_state{listenSocket = LSocket}, 
                    {ok, accept(NewState)};
                {error, Reason} ->
                    ?ERROR("gen_tcp:listen port(~p) is error: ~s~n", [Port, file:format_error(Reason)]),
                    {stop, Reason}
            end,
    Reply.

handle_cast({accepted, _Pid}, State=#socket_state{}) ->  
    {noreply, accept(State)}.  
handle_call(_Msg, _Caller, State) -> {noreply, State}.  
handle_info(_Msg, Library) -> {noreply, Library}.  
terminate(_Reason, _Library) -> ok.  
code_change(_OldVersion, Library, _Extra) -> {ok, Library}. 

accept(#socket_state{listenSocket = LSocket, protocol = Protocol}=State) ->
    proc_lib:spawn_link(?MODULE, accept_loop, [{self(), LSocket, Protocol}]), 
    State.

accept_loop({Server, LSocket, Protocol}) ->
    case gen_tcp:accept(LSocket) of
        {ok, Socket} ->
            try Protocol:start_link(Socket) of
                {ok, Pid} ->
                    shoot(Socket, Pid),
                    gen_server:cast(Server, {accepted, self()});
                _Other ->
                    gen_tcp:close(Socket)
            catch Class:Reason ->
                ?ERROR("accept_loop catch ~p:~p~n", [Class, Reason])
            end;
        {error, Reason_2} ->
            ?ERROR("gen_tcp:accept is error: ~s~n", [file:format_error(Reason_2)])
    end.


%% @doc Assigns a new controlling process Pid to Socket
shoot(Socket, Pid) ->
    case gen_tcp:controlling_process(Socket, Pid) of
        ok ->
            ok;
        {error, Reason} ->
            ?ERROR("gen_tcp:controlling_process is error: ~s~n", [file:format_error(Reason)]),
            gen_tcp:close(Socket),
            %% Only kill the supervised pid, because the connection's pid,
            %% when different, is supposed to be sitting under it and linked.
            exit(Pid, kill)
    end.
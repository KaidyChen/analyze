-module (analyze_build).

-include("analyze.hrl").
-include("analyze_config.hrl").
-include("print.hrl").

-behavisor(gen_server).

%% API
-export([start_link/2]).

%% Gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
]).

-record(state, {build_id}).

%%%==================================================================
%%% API
%%%==================================================================

start_link(Build_id, CallBackModule) ->
    gen_server:start_link(CallBackModule, [Build_id], []).

%%%==================================================================
%%% gen_server callbacks
%%%==================================================================

%%-------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server
%%
%% @spec init(Args) -> {ok, State} | 
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%-------------------------------------------------------------------
init([Build_id]) ->
    erlang:process_flag(trap_exit, true),
    analyze_build_pid:insert(Build_id, self()),
    % ?PRINT("Build_id:~p Child_build_id_list:~p Meter_field_list:~p ~n", [Build_id, Child_build_id_list, Meter_field_list]),
    State = #state{
        build_id = Build_id},
    {ok, State}.

%%-------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) -> 
%%                                      {reply, Reply, State} | 
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc
%%-------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%-------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% 
%% @spec handle_cast(Msg, State) ->
%%                          {noreply, State} |
%%                          {noreply, State, Timeout} |
%%                          {stop, Reason, State}
%% @end
%%-------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%-------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) ->
%%                          {noreply, State} |
%%                          {noreply, State, Timeout} |
%%                          {stop, Reason, State}
%% @end
%%-------------------------------------------------------------------
handle_info({'EXIT', _From, Reason}, State) ->
    {stop, Reason, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%-------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. when it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%-------------------------------------------------------------------
terminate(_Reason, State = #state{build_id = Build_id}) ->
    analyze_build_pid:delete(Build_id),
    ok.

%%-------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% 
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%-------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


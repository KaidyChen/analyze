-module(analyze_crontab_server).

-behaviour(gen_server).

-include("print.hrl").
-include("analyze_config.hrl").

%% Public API
-export([start_link/0]).

-export([start_all_tasks/0, start_task_by_name/1]).

%% gen_server callback API
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
]).

-define(SERVER, ?MODULE).

-record(state, {config_file, mtime, crontab_name_list}).

-define(TIMEOUT, 60 * 1000).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start_task_by_name(Name) ->
    gen_server:cast(?SERVER, {start_task_by_name, Name}).

start_all_tasks() ->
    gen_server:cast(?SERVER, start_all_tasks).

init([]) ->
    State = #state{config_file = ?CRONTAB_FILE, mtime = 0, crontab_name_list = []},
    % analyze_meter_crontab:init(),
    {ok, State, 0}.

handle_call(_Request, _From, State) ->
    {noreply, State, ?TIMEOUT}.

handle_cast(start_all_tasks, State = #state{crontab_name_list = Crontab_name_list}) ->
    erlang:spawn_link(fun() -> start_all_tasks(Crontab_name_list) end),
    {noreply, State, ?TIMEOUT};
handle_cast({start_task_by_name, Name}, State = #state{crontab_name_list = Crontab_name_list}) ->
    erlang:spawn_link(fun() -> start_task_by_name(Name, Crontab_name_list) end),
    {noreply, State, ?TIMEOUT};
handle_cast(_Msg, State) ->
    {noreply, State, ?TIMEOUT}.

handle_info(timeout, #state{mtime = 0, config_file = Config_File} = State) ->
    case file:consult(Config_File) of
        {ok, Crontab_List}->
            Crontab_name_list = lists:filtermap(fun add_crontab/1, Crontab_List),
            NewState = State#state{mtime = filelib:last_modified(Config_File), crontab_name_list = Crontab_name_list},
            {noreply, NewState, ?TIMEOUT};
        {error, Reason} ->
            ?ERROR("file:consult(~p) is error:~p", [Config_File, Reason]),
            {stop, normal, State}
    end;

handle_info(timeout, #state{mtime = MTime, config_file = Config_File, crontab_name_list = Crontab_name_list} = State) ->
    MTimeNew = filelib:last_modified(Config_File),
    NewState = case MTimeNew > MTime of
        true ->
            case file:consult(Config_File) of
                {ok, Crontab_List}->
                    remove_all_crontab(Crontab_name_list),
                    New_crontab_name_list = lists:filtermap(fun add_crontab/1, Crontab_List),
                    State#state{mtime = filelib:last_modified(Config_File), crontab_name_list = New_crontab_name_list};
                {error, Reason} ->
                    ?ERROR("file:consult(~p) is error:~p", [Config_File, Reason]),
                    State
            end;
        false ->
            State
    end,
    {noreply, NewState, ?TIMEOUT};

handle_info(_Msg, State) -> {noreply, State, ?TIMEOUT}.

terminate(_Reason, _State) -> ok.  

code_change(_OldVersion, State, _Extra) -> {ok, State, ?TIMEOUT}. 

add_crontab({Name, Spec, MFA = {M, F, A}}) ->
    case code:ensure_loaded(M) of
        {module, M} ->
            case crontab:add(Name, Spec, MFA) of
                ok ->
                    {true, {Name, MFA}};
                _ ->
                    false
            end;
        {error, What} ->
            ?ERROR("~p is unloaded: ~p~n", [M, What])
    end.
    
remove_all_crontab(Crontab_name_list) ->
    [crontab:remove(Name) || {Name, _MFA} <- Crontab_name_list].


start_all_tasks([{_Name, {M, F, A}} | Crontab_name_list]) ->
    erlang:apply(M, F, A),
    start_all_tasks(Crontab_name_list);
start_all_tasks([]) ->
    ok.


start_task_by_name(Name, [{Name, {M, F, A}} | Crontab_name_list]) ->
    erlang:apply(M, F, A);
start_task_by_name(Name, [_ | Crontab_name_list]) ->
    start_task_by_name(Name, Crontab_name_list);
start_task_by_name(_, []) ->
    ok.










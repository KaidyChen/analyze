-module(analyze_build_sup).

-behavisor(supervisor).

-include("print.hrl").

-export([
         start_link/0,
         init/1
        ]).

-export([
         start_build/1,
         start_build_room/1,
         stop_build/1
        ]).

-define(SERVER, ?MODULE).

start_link() ->  
    Args = [],
    Ret = supervisor:start_link({local, ?SERVER}, ?MODULE, Args),
    create_processes(),
    Ret.

init(_Args) ->
    init_relationship(),
    SupFlags = #{strategy => simple_one_for_one, intensity => 5, period => 10},
    ChildSpecs = [#{id => analyze_build,
                       start => {analyze_build, start_link, []},
                       restart => 'transient',
                       shutdown => 2000,
                       type => worker,
                       modules => 'dynamic'
                      }],
    {ok, {SupFlags, ChildSpecs}}.

create_processes() ->
    Fun =
        fun(Item, {Count, Start}) ->
                Room_id = analyze_build_meter:get_build_id(Item),
                try ?MODULE:start_build_room(Room_id) of
                    {ok, _Pid} ->
                        {Count+1, Start+1};
                    {error, Reason} ->
                        ?ERROR("~p spawn error:~p~n", [Room_id, Reason]), 
                        {Count+1, Start}
                catch 
                    _Class:Reason ->
                        ?ERROR("~p spawn error:~p~n", [Room_id, Reason]), 
                        {Count+1, Start}
                end
        end,
    {Count, Start} = analyze_build_meter:foldl(Fun, {0, 0}),
    ?PRINT("Count ~p Build of meters is Started ~p~n", [Count, Start]),
    ok.

start_build(Build_id) ->
    start_build(Build_id, analyze_build).

start_build_room(Build_id) ->
    start_build(Build_id, analyze_build_room).

start_build(Build_id, CallBackModule) ->
    case analyze_build_pid:lookup(Build_id) of
        {ok, Pid} ->
            case erlang:is_process_alive(Pid) of
                true ->
                    {error, started};
                false ->
                    supervisor:start_child(?MODULE, [Build_id, CallBackModule])
            end;
        _NotStarted ->
            {ok, Pid} = supervisor:start_child(?MODULE, [Build_id, CallBackModule]),
            {ok, Pid}
    end.

stop_build(Build_id) ->
    case analyze_build_pid:lookup(Build_id) of
        {ok, Pid} ->
            ok = supervisor:terminate_child(?MODULE, Pid);            
        _NotStarted ->
            {error, stoped}
    end.
            
%% Internal functions

init_relationship() ->
    analyze_build_pid:init(),
    analyze_build_time_table_store:load_time_table(),
    ok.





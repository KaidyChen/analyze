-module(analyze_gateway_sup).

-behavisor(supervisor).

-include("print.hrl").

-export([
         start_link/0,
         init/1
        ]).

-export([
         start_gateway/1,
         stop_gateway/1
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
    ChildSpecs = [#{id => analyze_gateway,
                       start => {analyze_gateway, start_link, []},
                       restart => 'transient',
                       shutdown => 5000,
                       type => worker,
                       modules => 'dynamic'
                      }],
    {ok, {SupFlags, ChildSpecs}}.

create_processes() ->    
    Fun =
        fun(Item, {Count, Start}) ->
                Gateway = analyze_gateway_meter:get_gateway(Item),
                case ?MODULE:start_gateway(Gateway) of
                    {ok, _Pid} ->
                        {Count+1, Start+1};
                    {error, Reason} ->
                        ?ERROR("~p spawn error:~p~n", [Gateway, Reason]), 
                        {Count+1, Start}
                end
        end,
    {Count, Start} = analyze_gateway_meter:foldl(Fun, {0, 0}),
    ?PRINT("Count ~p Gateway of meters is Started ~p~n", [Count, Start]),
    ok.

start_gateway(Gateway) ->
    case analyze_gateway_pid:lookup(Gateway) of
        {ok, Pid} ->
            case erlang:is_process_alive(Pid) of
                true ->
                    {error, started};
                false ->
                    supervisor:start_child(?MODULE, [Gateway])
            end;
        _NotStarted ->
            {ok, Pid} = supervisor:start_child(?MODULE, [Gateway]),
            {ok, Pid}
    end.

stop_gateway(Gateway) ->
    case analyze_gateway_pid:lookup(Gateway) of
        {ok, Pid} ->
            ok = supervisor:terminate_child(?MODULE, Pid);            
        _NotStarted ->
            {error, stoped}
    end.
            
%% Internal functions

init_relationship() ->
    analyze_gateway_pid:init(),
    analyze_gateway_status:init(),
    ok.

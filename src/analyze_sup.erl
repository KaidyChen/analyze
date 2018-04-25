-module(analyze_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Mod, Type), {Mod, {Mod, start_link, []}, permanent, 5000, Type, [Mod]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    {ok, Pid}.

%%start_child(ChildSpec) when is_tuple(ChildSpec) ->
    %%supervisor:start_child(?MODULE, ChildSpec).

%%-spec(start_child(Mod :: atom(), Type :: supervisor | worker) -> {ok, pid()}).
%%start_child(Mod, Type) when is_atom(Mod), is_atom(Type) ->
    %%supervisor:start_child(?MODULE, ?CHILD(Mod, Type)).

%% ===================================================================
%% End API functions
%% ===================================================================


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, []} }.

%% ===================================================================
%% End Supervisor callbacks
%% ===================================================================

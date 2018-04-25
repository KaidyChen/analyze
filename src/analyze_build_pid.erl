-module (analyze_build_pid).

%% API
-export([init/0]).

-export([insert/2,
         lookup/1,
         delete/1
]).

-define(TABLE_ID, ?MODULE).

%%%======================================================
%%% API
%%%======================================================

%%-------------------------------------------------------
%% @doc
%% Initiates ets
%% 
%% @spec init() -> void()
%% @end
%%-------------------------------------------------------
init() ->
    ets:new(?TABLE_ID, [set, public, named_table]),
    ok.

%%-------------------------------------------------------
%% @doc
%% Insert value(Build_id, Pid) to ets
%%
%% @spec insert(Build_id, Pid) -> void()
%% @end
insert(Build_id, Pid) ->
    ets:insert(?TABLE_ID, {Build_id, Pid}),
    ok.

%%-------------------------------------------------------
%% @doc
%% Find a Pid give a Build_id
%%
%% @spec lookup(Build_id) -> {ok, Pid} | {error, not_found}
%% @end
lookup(Build_id) ->
    case ets:lookup(?TABLE_ID, Build_id) of
        [{Build_id, Pid}] ->
            {ok, Pid};
        [] ->
            {error, not_found}
    end.

%%--------------------------------------------------------
%% @doc
%% Delete a gateway from ets
%%
%% @spec delete(Build_id) -> void()
%%--------------------------------------------------------
delete(Build_id) ->
    ets:delete(?TABLE_ID, Build_id).


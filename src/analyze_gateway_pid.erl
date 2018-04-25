-module(analyze_gateway_pid).

%% API
-export([init/0]).

-export([insert/2,
         lookup/1,
         lookup_by_pid/1,
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
%% Insert value(Gateway_Code, Pid) to ets
%%
%% @spec insert(Gateway_Code, Pid) -> void()
%% @end
insert(Gateway_Code, Pid) ->
    ets:insert(?TABLE_ID, {Gateway_Code, Pid}),
    ok.

%%-------------------------------------------------------
%% @doc
%% Find a Pid give a Gateway_Code
%%
%% @spec lookup(Gateway_Code) -> {ok, Pid} | {error, not_found}
%% @end
lookup(Gateway_Code) ->
    case ets:lookup(?TABLE_ID, Gateway_Code) of
        [{Gateway_Code, Pid}] ->
            {ok, Pid};
        [] ->
            {error, not_found}
    end.

lookup_by_pid(Pid) ->
    case ets:match_object(?TABLE_ID, {'$1', Pid}) of
        [{Gateway, Pid}|_] ->
            {ok, Gateway};
        [] -> 
            {error, not_found}
    end.


%%--------------------------------------------------------
%% @doc
%% Delete a gateway from ets
%%
%% @spec delete(Gateway_Code) -> void()
%%--------------------------------------------------------
delete(Gateway_Code) ->
    ets:delete(?TABLE_ID, Gateway_Code).








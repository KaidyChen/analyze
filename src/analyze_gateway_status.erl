-module(analyze_gateway_status).

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
%% Insert value(Gateway_Code, Status) to ets
%%
%% @spec insert(Gateway_Code, Status) -> void()
%% @end
insert(Gateway_Code, Status) ->
    ets:insert(?TABLE_ID, {Gateway_Code, Status}),
    ok.

%%-------------------------------------------------------
%% @doc
%% Find a Status give a Gateway_Code
%%
%% @spec lookup(Gateway_Code) -> {ok, Status} | {error, not_found}
%% @end
lookup(Gateway_Code) ->
    case ets:lookup(?TABLE_ID, Gateway_Code) of
        [{Gateway_Code, Status}] ->
            {ok, Status};
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


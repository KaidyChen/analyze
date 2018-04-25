-module(analyze_meter_gateway).

%% API
-export([init/0]).

-export([insert/3,
         insert_list/1,
         lookup/2,
         get_meter_and_type_list_by_gateway/1,
         delete/2
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
%% Insert value to ets
%%
%% @spec insert(Meter, Meter_type, Gateway) -> void()
%% @end
insert(Meter, Meter_type, Gateway) ->
    ets:insert(?TABLE_ID, {{Meter, Meter_type}, Gateway}),
    ok.

insert_list(Meter_and_type_and_gateway_list) ->
    ets:insert(?TABLE_ID, Meter_and_type_and_gateway_list),
    ok.

%%-------------------------------------------------------
%% @doc
%% Find a gateway give a meter
%%
%% @spec lookup(Meter, Meter_type) -> {ok, Gateway} | {error, not_found}
%% @end
lookup(Meter, Meter_type) ->
    case ets:lookup(?TABLE_ID, {Meter, Meter_type}) of
        [{_, Gateway}] ->
            {ok, Gateway};
        [] ->
            {error, not_found}
    end.

get_meter_and_type_list_by_gateway(Gateway) ->
    case ets:match(?TABLE_ID, {'$1', Gateway}) of
        [] -> {error, not_found};
        Meter_and_type_list -> {ok, Meter_and_type_list}    
    end.

%%--------------------------------------------------------
%% @doc
%% Delete a meter from ets
%%
%% @spec delete(Meter, Meter_type) -> void()
%%--------------------------------------------------------
delete(Meter, Meter_type) ->
    ets:delete(?TABLE_ID, {Meter, Meter_type}).







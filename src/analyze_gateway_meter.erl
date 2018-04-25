-module (analyze_gateway_meter).

%% API
-export([init/0]).

-export([insert/2,
         insert/1,
         lookup/1,
         delete/1,
         empty/0,
         foldl/2,
         get_gateway_id_list/0,
         get_gateway/1,
         tab2list/0
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
    ets:new(?TABLE_ID, [set, protected, named_table]),
    ok.


empty() ->
    ets:delete_all_objects(?TABLE_ID).

%%-------------------------------------------------------
%% @doc
%% Insert value to ets
%%
%% @spec insert(gateway_id, Meter_type_and_meter_list) -> void()
%% @end
insert(Gateway, Meter_type_and_meter_list) ->
    ets:insert(?TABLE_ID, {Gateway, Meter_type_and_meter_list}),
    ok.

insert(GatewayAndMeterTypeMeterList) when is_list(GatewayAndMeterTypeMeterList) ->
    ets:insert(?TABLE_ID, GatewayAndMeterTypeMeterList).

%%-------------------------------------------------------
%%
%% @spec lookup(Gateway) -> {ok, Meter_type_and_meter_list} | {error, not_found}
%% @end
lookup(Gateway) ->
    case ets:lookup(?TABLE_ID, Gateway) of
        [{Gateway, Meter_type_and_meter_list}] ->
            {ok, Meter_type_and_meter_list};
        [] ->
            {error, not_found}
    end.

%%--------------------------------------------------------
%% @doc
%% Delete a gateway from ets
%%
%% @spec delete(Gateway) -> void()
%%--------------------------------------------------------
delete(Gateway) ->
    ets:delete(?TABLE_ID, Gateway).

%% ets表转为list
tab2list() ->
    ets:tab2list(?TABLE_ID).

get_gateway_id_list() ->
    Fun = 
        fun({Gateway, _}, Acc0) ->
                [Gateway | Acc0]
        end,
    foldl(Fun, []).

foldl(Fun, Acc0) ->
    ets:foldl(Fun, Acc0, ?TABLE_ID).

get_gateway({Gateway, _}) ->
    Gateway.





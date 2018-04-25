-module(analyze_meter_to_cq).

%% API
-export([init/0]).

-export([insert/3,
         lookup/2,
         lookup/1,
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
%% Insert value(Meter, Cq) to ets
%%
%% @spec insert(Meter, Cq) -> void()
%% @end
insert(Meter_type, Meter, Cq) ->
    ets:insert(?TABLE_ID, {{Meter_type, Meter}, Cq}),
    ok.

%%-------------------------------------------------------
%% @doc
%% Find a Cq give a meter
%%
%% @end
lookup(Meter_type, Meter) ->
    lookup({Meter_type, Meter}).
lookup({Meter_type, Meter}) ->
    case ets:lookup(?TABLE_ID, {Meter_type, Meter}) of
        [{{Meter_type, Meter}, Cq}] ->
            {ok, Cq};
        [] ->
            {error, not_found}
    end.

%%--------------------------------------------------------
%% @doc
%% Delete a meter from ets
%%
%%--------------------------------------------------------
delete(Meter_type, Meter) ->
    ets:delete(?TABLE_ID, {Meter_type, Meter}).

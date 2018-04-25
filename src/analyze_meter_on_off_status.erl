-module(analyze_meter_on_off_status).

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

insert(Meter_type, Meter, OnOffStatus) ->
    ets:insert(?TABLE_ID, {{Meter_type, Meter}, OnOffStatus}),
    ok.

%%-------------------------------------------------------
%% @doc
%%
%% @end
lookup(Meter_type, Meter) ->
    lookup({Meter_type, Meter}).
lookup({Meter_type, Meter}) ->
    case ets:lookup(?TABLE_ID, {Meter_type, Meter}) of
        [{{Meter_type, Meter}, OnOffStatus}] ->
            {ok, OnOffStatus};
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


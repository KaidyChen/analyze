-module (analyze_build_meter).

%% API
-export([init/0]).

-export([insert/2,
         insert/1,
         lookup/1,
         delete/1,
         empty/0,
         get_build_id_list/0,
         foldl/2,
         get_build_id/1,
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
%% @spec insert(Build_id, Meter_field_list) -> void()
%% @end
insert(Build_id, Meter_field_list) ->
    ets:insert(?TABLE_ID, {Build_id, Meter_field_list}),
    ok.

insert(BuildIdAndMeterFieldList) when is_list(BuildIdAndMeterFieldList) ->
    ets:insert(?TABLE_ID, BuildIdAndMeterFieldList).

%%-------------------------------------------------------
%% @doc
%% Find a Meter_field_list give a Build_id
%%
%% @spec lookup(Build_id) -> {ok, Meter_field_list} | {error, not_found}
%% @end
lookup(Build_id) ->
    case ets:lookup(?TABLE_ID, Build_id) of
        [{Build_id, Meter_field_list}] ->
            {ok, Meter_field_list};
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

%% ets表转为list
tab2list() ->
    ets:tab2list(?TABLE_ID).

get_build_id_list() ->
    Fun = 
        fun({Build_id, _}, Acc0) ->
                [Build_id | Acc0]
        end,
    ets:foldl(Fun, [], ?TABLE_ID).

foldl(Fun, Acc0) ->
    ets:foldl(Fun, Acc0, ?TABLE_ID).

get_build_id({Build_id, _}) ->
    Build_id.









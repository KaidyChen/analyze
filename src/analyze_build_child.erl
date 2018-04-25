-module (analyze_build_child).

%% API
-export([init/0]).

-export([insert/2,
         insert/1,
         lookup/1,
         delete/1,
         empty/0,
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
    ets:new(?TABLE_ID, [set, public, named_table]),
    ok.

empty() ->
    ets:delete_all_objects(?TABLE_ID).

%%-------------------------------------------------------
%% @doc
%% Insert value to ets
%%
%% @spec insert(Build_id, Child_build_id_list) -> void()
%% @end
insert(Build_id, Child_build_id_list) ->
    ets:insert(?TABLE_ID, {Build_id, Child_build_id_list}),
    ok.

insert(ObjList) when is_list(ObjList) ->
    ets:insert(?TABLE_ID, ObjList).

%%-------------------------------------------------------
%% @doc
%% Find a Child_build_id_list give a Build_id
%%
%% @spec lookup(Build_id) -> {ok, Child_build_id_list} | {error, not_found}
%% @end
lookup(Build_id) ->
    case ets:lookup(?TABLE_ID, Build_id) of
        [{Build_id, Child_build_id_list}] ->
            {ok, Child_build_id_list};
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







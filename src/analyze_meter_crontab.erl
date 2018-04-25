-module (analyze_meter_crontab).

-export([init/0,
         insertAll/1,
         lookup/1,
         tab2list/0,
         delete_all_objects/0
]).

-define(TABLE_ID, ?MODULE).

init() ->
    ets:new(?TABLE_ID, [bag, public, named_table]),
    ok.

insertAll(Terms) when is_list(Terms) ->
    ets:insert(?TABLE_ID, Terms).

lookup(Type) ->
    case ets:lookup(?TABLE_ID, Type) of
        [{Type, Tasks_List}] ->
            {ok, Tasks_List};
        [] ->
            {error, not_found}
    end.

tab2list() ->
    ets:tab2list(?TABLE_ID).

delete_all_objects() ->
    ets:delete_all_objects(?TABLE_ID).

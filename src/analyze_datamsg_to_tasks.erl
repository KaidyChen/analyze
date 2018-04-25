-module(analyze_datamsg_to_tasks).

-export([init/0,
         insertAll/1,
         lookup/1
]).

-define(TABLE_ID, ?MODULE).

init() ->
    ets:new(?TABLE_ID, [set, public, named_table]),
    ok.

insertAll(Trigger_tasks_list) when is_list(Trigger_tasks_list) ->
    ets:insert(?TABLE_ID, Trigger_tasks_list).

lookup(Type_Tuple) ->
    case ets:lookup(?TABLE_ID, Type_Tuple) of
        [{Type_Tuple, Tasks_List}] ->
            {ok, Tasks_List};
        [] ->
            {error, not_found}
    end.

%%insert(Type_Tuple, Tasks_List) when is_tuple(Type_Tuple) andalso is_list(Tasks_List) ->
    %%ets:insert(?TABLE_ID, {Type_Tuple, Tasks_List}).


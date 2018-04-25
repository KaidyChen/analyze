-module (analyze_meter_field_store).

-include("analyze.hrl").

%% API
-export([init/0]).

-export([
         insert/1,
         delete/1,
         lookup/2,
         lookup/1,
         match_object/1
        ]).

-export([
         get_meter_classify_by_gateway/0,
         get_meter_classify_by_build_id/0,
         get_keys_of_meter_classify/1,
         meter_classify_to_list/1
        ]).

-export([
         each/1,
         get_gateway_set/0,
         get_meter_classify_map/0,
         get_gateway/2,
         foldl/2,
         get_meter_and_type_list_by_gateway/1,
         get_meter_field_list/1
        ]).


-define(TABLE_ID, ?MODULE).


init() ->
    Keypos = analyze_meter_field:get_meter_field_keypos(),
    ets:new(?TABLE_ID, [named_table, set, public, {keypos, Keypos}, {read_concurrency, true}]),
    ok.

insert(ObjOrObjs) ->
    ets:insert(?TABLE_ID, ObjOrObjs),
    ok.

delete(Key) ->
    ets:delete(?TABLE_ID, Key),
    ok.

lookup(Meter_type, Meter) ->
    lookup({Meter_type, Meter}).

lookup({Meter_type, Meter} = Key) ->
    case ets:lookup(?TABLE_ID, Key) of
        [Meter_field] ->
            {ok, Meter_field};
        [] ->
            {error, mot_found}
    end.

match_object('$end_of_table') ->
    {error, eof};
match_object(Limit) when is_integer(Limit), Limit > 0 ->
    case ets:match_object(?TABLE_ID, '$1', Limit) of
        {M, C} ->
            {ok, M, C};
        '$end_of_table' ->
            {error, eof}
    end;
match_object(Continuation) ->
    case ets:match_object(Continuation) of
        {M, C} ->
            {ok, M, C};       
        '$end_of_table' ->
            {error, eof}
    end.
            

foldl(Fun, Acc0) ->
    ets:foldl(Fun, Acc0, ?TABLE_ID).

get_keys_of_meter_classify(Meter_classify) ->
    dict:fetch_keys(Meter_classify).

meter_classify_to_list(Meter_classify) ->
    dict:to_list(Meter_classify).

get_meter_classify_by_build_id() ->
    FunOfGetKey = fun analyze_meter_field:get_build_id_by_meter_field/1,
    FunOfGetValue = fun analyze_meter_field:get_meter_type_and_meter_by_meter_field/1,
    get_meter_classify_by_xx(FunOfGetKey, FunOfGetValue).
    
get_meter_classify_by_gateway() ->
    FunOfGetKey = fun analyze_meter_field:get_gateway_by_meter_field/1,
    FunOfGetValue = fun analyze_meter_field:get_meter_type_and_meter_by_meter_field/1,
    get_meter_classify_by_xx(FunOfGetKey, FunOfGetValue).

get_meter_classify_by_xx(FunOfGetKey, FunOfGetValue) ->
    Fun = 
        fun(Meter_field, Dict) ->
                Key = FunOfGetKey(Meter_field),
                Value = FunOfGetValue(Meter_field),
                case dict:is_key(Key, Dict) of
                    true ->
                        dict:append(Key, Value, Dict);
                    false ->
                        dict:store(Key, [Value], Dict)
                end
        end,
    foldl(Fun, dict:new()).
    
each(Fun) ->
    ets:safe_fixtable(?TABLE_ID, true),
    each_(?TABLE_ID, ets:first(?TABLE_ID), Fun),
    ets:safe_fixtable(?TABLE_ID, false),
    ok.

each_(_Tab, '$end_of_table', _) ->
    ok;
each_(Tab, Key, Fun) ->
    case lookup(Key) of
        {ok, MeterField} ->
            Fun(MeterField);
        _ ->
            ok
    end,
    each_(Tab, ets:next(Tab, Key), Fun).

get_gateway_set() ->
    ets:safe_fixtable(?TABLE_ID, true),
    Gateway_set = get_gateway_set_(?TABLE_ID, ets:first(?TABLE_ID), sets:new()),
    ets:safe_fixtable(?TABLE_ID, false),
    Gateway_set.

get_gateway_set_(_Tab, '$end_of_table', Set) ->
    Set;
get_gateway_set_(Tab, Key, Set) ->
    NewSet = 
        case lookup(Key) of
            {ok, Meter_field} ->
                Gateway = analyze_meter_field:get_gateway_by_meter_field(Meter_field),
                sets:add_element(Gateway, Set);
            _ ->
                Set
        end,
    get_gateway_set_(Tab, ets:next(Tab, Key), NewSet).
            
%% 更新Map的键值对, 
%% 若先前不存在键值Key, 则把[Element]列表作为Value，插入Map
%% 若已存在Key，则把[Element | Old_value]作为新的Value，插入Map
update_map(Key, Element, Map) ->
    case maps:is_key(Key, Map) of
        false -> 
            Value = [Element],
            maps:put(Key, Value, Map);
        true ->
            Old_value = maps:get(Key, Map),
            Value = [Element | Old_value],
            maps:put(Key, Value, Map)
    end.

get_meter_classify_map() ->
    ets:safe_fixtable(?TABLE_ID, true),
    Meter_classify_map = get_meter_classify_map_(?TABLE_ID, ets:first(?TABLE_ID), maps:new()),
    ets:safe_fixtable(?TABLE_ID, false),
    Meter_classify_map.

get_meter_classify_map_(_Tab, '$end_of_table', Map) ->
    Map;
get_meter_classify_map_(Tab, Key, Map) ->
    NewMap = 
        case lookup(Key) of
            {ok, Meter_field} ->
                Build_id = analyze_meter_field:get_build_id_by_meter_field(Meter_field),
                update_map(Build_id, Meter_field, Map);
            _ ->
                Map
        end,
    get_meter_classify_map_(Tab, ets:next(Tab, Key), NewMap).

get_gateway(Meter_type, Meter) ->
    case ets:lookup(?TABLE_ID, {Meter_type, Meter}) of
        [Meter_field] ->
            Gateway = analyze_meter_field:get_gateway_by_meter_field(Meter_field),
            {ok, Gateway};
        [] ->
            {error, not_found}
    end.

get_meter_and_type_list_by_gateway(Gateway) ->
    Meter_field = analyze_meter_field:get_match_meter_and_meter_type_meter_field_by_gateway(Gateway),
    case ets:match(?TABLE_ID, Meter_field) of
        [] -> {error, not_found};
        Meter_and_type_list -> {ok, Meter_and_type_list}    
    end.

get_meter_field_list(Limit) when is_integer(Limit) ->
    case ets:match(?TABLE_ID, '$1', Limit) of
        '$end_of_table' -> {[], done};
        {M, C} -> {M, C}
    end;
get_meter_field_list('$end_of_table') ->
    {[], done};
get_meter_field_list(C) ->
    case ets:match(?TABLE_ID, C) of
        {NewM, NewC} -> {NewM, NewC};
        _ -> {[], done}
    end.

   



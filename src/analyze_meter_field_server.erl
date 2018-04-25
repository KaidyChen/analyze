-module(analyze_meter_field_server).

-behavior(gen_server).

-include("analyze.hrl").
-include("analyze_config.hrl").
-include("print.hrl").

-export([start_link/0]).
-export([insert_meter_field/1, delete_meter_field/1]).

-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
         ]).

-define(SERVER, ?MODULE).
-define(LIMIT, 4).

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

insert_meter_field(MeterFieldOrList) ->
    gen_server:cast(?SERVER, {insert_meter_field, MeterFieldOrList}).

delete_meter_field({Meter_type, Meter} = Key) ->
    gen_server:cast(?SERVER, {delete_meter_field, Key}).

init([]) ->
    State = #state{},
    %% 创建ets表
    analyze_meter_field_store:init(),
    analyze_build_meter:init(),
    analyze_build_child:init(),
    analyze_gateway_meter:init(),

    %% 加载配表配置信息
    load_meter_config(),
    %% 将meter以建筑进行分类
    load_build_meter(),
    %% 将meter以网关进行分类
    load_gateway_meter(),
    {ok, State, hibernate}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast({insert_meter_field, MeterFieldOrList}, State) ->
    analyze_meter_field_store:insert(MeterFieldOrList),
    load_build_meter(),
    load_gateway_meter(),
    store_meter_field_to_disk(),
    {noreply, State};
handle_cast({delete_meter_field, {Meter_type, Meter} = Key}, State) ->
    analyze_meter_field_store:delete(Key),
    load_build_meter(),
    load_gateway_meter(),
    store_meter_field_to_disk(),
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

load_meter_config() ->
    case file:open(?METERS_CONFIG, [read, raw]) of
        {ok, IoDevice} ->
            read_line(IoDevice),
            file:close(IoDevice);
        {error, Reason} ->
            halt()
    end.

store_meter_field_to_disk() ->    
    case file:open(?METERS_CONFIG, [write]) of
        {ok, IoDevice} ->
            Fun = 
                fun(IoDevice, MeterFieldList) ->
                        MeterFieldDataLine = analyze_meter_field:get_meter_field_data_line_list(MeterFieldList),
                        io:fwrite(IoDevice, "~s~n", [MeterFieldDataLine])
                end,
            write_data(?LIMIT, IoDevice, Fun),
            file:close(IoDevice);
        {error, Reason} ->
            halt()
    end.
    

read_line(IoDevice) ->
    case file:read_line(IoDevice) of
        {ok, Data} ->
            handle_meter_field(Data),            
            read_line(IoDevice);
        eof ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

write_data(ContinuationOrLimit, IoDevice, Fun) ->
    case analyze_meter_field_store:match_object(ContinuationOrLimit) of
        {ok, M, C} ->
            Fun(IoDevice, M),
            write_data(C, IoDevice, Fun);
        {error, Reason} ->
            ok
    end.

handle_meter_field(MeterInfo) ->
    case analyze_meter_field:get_meter_field_by_meter_info(MeterInfo) of
        {ok, MeterField} ->
            analyze_meter_field_store:insert(MeterField);
        {error, Reason} ->
            ?ERROR("~p is error:~p~n", [MeterInfo, Reason]),
            ok
    end.

load_gateway_meter() ->
    Meter_classify_by_gateway = analyze_meter_field_store:get_meter_classify_by_gateway(),
    analyze_gateway_meter:empty(),
    analyze_gateway_meter:insert(analyze_meter_field_store:meter_classify_to_list(Meter_classify_by_gateway)),
    ok.
              
load_build_meter() ->

    Meter_classify_by_build_id = analyze_meter_field_store:get_meter_classify_by_build_id(),
    analyze_build_meter:empty(),
    analyze_build_meter:insert(analyze_meter_field_store:meter_classify_to_list(Meter_classify_by_build_id)),
   
    Build_id_list = analyze_meter_field_store:get_keys_of_meter_classify(Meter_classify_by_build_id),
    {Room_id_list, Floor_id_list, Building_id_list, Garden_id_list} = get_classify_build_id_list(Build_id_list),    
    
    Floor_id_to_room_id_list = get_upper_id_to_build_id_list(Room_id_list),
    Building_id_to_floor_id_list = get_upper_id_to_build_id_list(
                                     lists:merge(Floor_id_list, proplists:get_keys(Floor_id_to_room_id_list))),
    Garden_id_to_building_id_list = get_upper_id_to_build_id_list(
                                      lists:merge(Building_id_list, proplists:get_keys(Building_id_to_floor_id_list))),

    analyze_build_child:empty(),
    lists:foreach(fun(ObjList) ->
                          analyze_build_child:insert(ObjList)
                  end, [Floor_id_to_room_id_list, Building_id_to_floor_id_list, Garden_id_to_building_id_list]).

get_classify_build_id_list(Build_id_list) ->
    Fun = fun
        (Build_id, {Room_id_list, Floor_id_list, Building_id_list, Garden_id_list}) ->
            case length(Build_id) of
                ?ROOM_ID_LEN -> 
                    {[Build_id | Room_id_list], Floor_id_list, Building_id_list, Garden_id_list};
                ?FLOOR_ID_LEN ->
                    {Room_id_list, [Build_id | Floor_id_list], Building_id_list, Garden_id_list};
                ?BUILDING_ID_LEN ->
                    {Room_id_list, Floor_id_list, [Build_id | Building_id_list], Garden_id_list};
                ?GARDEN_ID_LEN ->
                    {Room_id_list, Floor_id_list, Building_id_list, [Build_id | Garden_id_list]};
                _ ->
                    {Room_id_list, Floor_id_list, Building_id_list, Garden_id_list}     
            end
    end,
    lists:foldl(Fun, {[], [], [], []}, Build_id_list).

get_upper_id_to_build_id_list(Build_id_list) ->
    Fun = 
        fun(Build_id) ->
                Upper_build_id = get_upper_id(Build_id),
                {Upper_build_id, Build_id}
        end,
    List1 = lists:map(Fun, Build_id_list),
    [{K, proplists:get_all_values(K, List1)} || K <- proplists:get_keys(List1)].

%% 从建筑Id获取它上层的建筑Id
get_upper_id(Build_id) when (length(Build_id) =:= ?ROOM_ID_LEN) ->
    lists:sublist(Build_id, ?FLOOR_ID_LEN);
get_upper_id(Build_id) when (length(Build_id) =:= ?FLOOR_ID_LEN) ->
    lists:sublist(Build_id, ?BUILDING_ID_LEN);
get_upper_id(Build_id) when (length(Build_id) =:= ?BUILDING_ID_LEN) ->
    lists:sublist(Build_id, ?GARDEN_ID_LEN).


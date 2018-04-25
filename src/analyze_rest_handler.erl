-module(analyze_rest_handler).

-include("analyze.hrl").
-include("print.hrl").

-include("analyze_store.hrl").

-export([init/2]).
-export([allowed_methods/2]).
-export([allow_miss_port/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).

-define(SUCCESS_STATUS, <<"200">>).
-define(FAILURE_STATUS, <<"300">>).

-define(PATH_TO_FUN, 
        [
         {<<"addLightingStrategy">>, add_lighting_strategy},
         {<<"touchLightingStrategy">>, touch_lighting_strategy},
         {<<"deleteLightingStrategy">>, delete_lighting_strategy},
         {<<"batchAddLightingStrategy">>, batch_add_lighting_strategy},
         {<<"batchDeleteLightingStrategy">>, batch_delete_lighting_strategy},
         {<<"hello">>, hello}
        ]).

-export([
         response_to_json/2, 
         response_to_html/2, 
         response_to_text/2
        ]).

-export([
         add_lighting_strategy/1,
         touch_lighting_strategy/1,
         delete_lighting_strategy/1,
         batch_add_lighting_strategy/1,
         batch_delete_lighting_strategy/1,
         hello/1
        ]).

init(Req, State) ->
    ?PRINT("REQ: ~p~n", [Req]),
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    Result = [<<"GET">>, <<"POST">>, <<"HEAD">>, <<"OPTIONS">>],
    {Result, Req, State}.

allow_miss_port(Req, State) ->
    Result = false,
    {Result, Req, State}.

content_types_provided(Req, State) ->
    Result = [
             % {<<"text/html">>,  response_to_html},
             % {<<"text/plain">>, response_to_text},
              {<<"application/x-www-form-urlencoded">>, response_to_json},
              {<<"application/json">>,  response_to_json}
             ],
    {Result, Req, State}.

content_types_accepted(Req, State) ->
    Result = [
              {<<"application/x-www-form-urlencoded">>, response_to_json},
              {<<"application/json">>,  response_to_json}
             ],
    {Result, Req, State}.

response_to_json(Req, State) ->
    ?PRINT("response: ~n", []),
    Path = cowboy_req:path(Req),
    Method = cowboy_req:method(Req),
    {NewReq, Qs} = 
        case cowboy_req:has_body(Req) of
            true ->
                {ok, Body, ReqTmp} = cowboy_req:read_body(Req),
                ?PRINT("Body: ~p~n", [Body]),
                {ReqTmp, jsx:decode(Body)};
            false ->
                {Req, cowboy_req:parse_qs(Req)}
        end,

    Result = get_result(Path, Qs),
    ?PRINT("Result:~s~n", [binary_to_list(Result)]),

    ?PRINT("Pid: ~p~n", [self()]),
    ?PRINT("Path: ~p~n", [Path]),
    ?PRINT("Qs: ~p~n", [Qs]),

    case Method of
        <<"POST">> ->
            Res1 = cowboy_req:set_resp_body(Result, NewReq),
            Res2 = cowboy_req:delete_resp_header(<<"content-type">>, Res1),
            Res3 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Res2),
            {true, Res3, State};
        _ ->
            {Result, NewReq, State}
    end.

response_to_html(Req, State) ->
    Result = <<"<html><body>
               <p>REST Hello World as HTML!</p>
               </body>
               </html>">>,
    {Result, Req, State}.

response_to_text(Req, State) ->
    Result = <<"Hello World!">>,
    {Result, Req, State}.

get_result(Path, Qs) ->
    Fun = proplists:get_value(filename:basename(Path), ?PATH_TO_FUN, hello),
    ?PRINT("Fun: ~p~n", [Fun]),
    apply(?MODULE, Fun, [Qs]).

hello(Qs) ->
    Result = <<"{\"rest\": \"Hello World！\"}">>.

format_return(Reason, ReturnCode) ->
    ReturnMsg = ?HELPER:to_iolist(Reason),
    jsx:encode([{<<"returnCode">>, ReturnCode}, {<<"returnMsg">>, ReturnMsg}]).

batch_add_lighting_strategy(Qs) ->
    case batch_add_lighting_strategy_(Qs, []) of
        {ok, Lighting_strategy_tuple_List} ->
            analyze_lighting_strategy_server:add_lighting_strategy(Lighting_strategy_tuple_List),
            format_return("batch add lighting strategy success", ?SUCCESS_STATUS);
        {error, Reason} ->
            format_return(Reason, ?FAILURE_STATUS)
    end.

batch_add_lighting_strategy_([Qs1 | T], Lighting_strategy_tuple_List) ->
    try parse_lighting_strategy(Qs1) of
        Lighting_strategy_tuple ->
            batch_add_lighting_strategy_(T, [Lighting_strategy_tuple | Lighting_strategy_tuple_List])
    catch
        _:Reason ->
            {error, Reason}
    end;
batch_add_lighting_strategy_([], Lighting_strategy_tuple_List) ->
    {ok, Lighting_strategy_tuple_List}.
    
batch_delete_lighting_strategy(Qs) ->
    case proplists:get_value(<<"taskIds">>, Qs) of
        undefined -> 
            format_return("taskIds field not found", ?FAILURE_STATUS);
        TaskIdsBitString ->
            TaskIdList = string:tokens(binary_to_list(TaskIdsBitString), ","),
            analyze_lighting_strategy_server:delete_lighting_strategy(TaskIdList),
            format_return("batch delete lighting strategy success", ?SUCCESS_STATUS)
    end.
    
add_lighting_strategy(Qs) ->
    try parse_lighting_strategy(Qs) of
        Lighting_strategy_tuple ->
            analyze_lighting_strategy_server:add_lighting_strategy(Lighting_strategy_tuple),
            format_return("add lighting strategy success", ?SUCCESS_STATUS)            
    catch
        _:Reason ->
            format_return(Reason, ?FAILURE_STATUS)
    end.

touch_lighting_strategy(Qs) ->
    case proplists:get_value(<<"taskId">>, Qs) of
        undefined -> 
            format_return("taskId field not found", ?FAILURE_STATUS);
        TaskIdBitString ->
            TaskId = binary_to_list(TaskIdBitString),
            try analyze_lighting_strategy_server:touch_lighting_strategy(TaskId) of
                ok ->
                    format_return("touch lighting strategy success", ?SUCCESS_STATUS);
                {error, Reason} ->
                    format_return(Reason, ?FAILURE_STATUS)
            catch
                _Class:Reason ->
                    format_return(Reason, ?FAILURE_STATUS)
            end
    end.
    
delete_lighting_strategy(Qs) ->
    case proplists:get_value(<<"taskId">>, Qs) of
        undefined -> 
            format_return("taskId field not found", ?FAILURE_STATUS);
        TaskIdBitString ->
            TaskId = binary_to_list(TaskIdBitString),
            analyze_lighting_strategy_server:delete_lighting_strategy(TaskId),
            format_return("delete lighting strategy success", ?SUCCESS_STATUS)
    end.
    
get_validity(Qs) ->
    ValidityStartStr = binary_to_list(proplists:get_value(<<"validityStart">>, Qs, <<>>)),
    ValidityEndStr = binary_to_list(proplists:get_value(<<"validityEnd">>, Qs, <<>>)),
    {?HELP:strToDate(ValidityStartStr), ?HELP:strToDate(ValidityEndStr)}.

get_time(Qs) ->
    case proplists:get_value(<<"timeList">>, Qs) of
        undefined ->
            throw("time field not found");
        TimeBitStringList ->
            Fun = 
                fun(TimeBitString) ->
                        TimeStr = binary_to_list(TimeBitString),
                        [Hour, Minute] = string:tokens(TimeStr, ":"),
                        {list_to_integer(Hour), list_to_integer(Minute)}
                end,
            lists:map(Fun, TimeBitStringList)
    end.

parse_lighting_strategy(Qs) ->
    TaskId = 
        case proplists:get_value(<<"taskId">>, Qs) of
            undefined -> 
                throw("taskId field not found");
            TaskIdTmp ->
                binary_to_list(TaskIdTmp)
        end,
    StrategyType = 
        case proplists:get_value(<<"strategyType">>, Qs) of
            undefined ->
                throw("strategyType field not found");
            StrategyTypeTmp ->
                binary_to_list(StrategyTypeTmp)
        end,

    ReissuedType = 
        case proplists:get_value(<<"reissuedType">>, Qs) of
            undefined ->
                {3, 1};
            IntervalAndRepeatTmp ->
                case string:tokens(binary_to_list(IntervalAndRepeatTmp), ",") of
                    [IntervalStr, RepeatStr] ->
                        case {(Interval = list_to_integer(IntervalStr)) >= 0, (Repeat = list_to_integer(RepeatStr)) >= 0} of
                            {true, true} ->
                                {Interval, Repeat};
                            _ ->
                                {3, 1}
                        end;
                    _ ->
                        {3, 1}
                end
        end,

    LightingDataList = get_lighting_data(Qs),
    CentralAirDataList = get_central_air_data(Qs),

    DataList = [{?LIGHTING_TYPE, LightingDataList}, {?CENTRAL_AC_TYPE, CentralAirDataList}],
   
    case StrategyType of
        ?TOUCH_TYPE ->
            {TaskId, StrategyType, ReissuedType, DataList};
        ?TIMER_TYPE ->
            {ValidityStart, ValidityEnd} = get_validity(Qs),
            CycleMode = binary_to_list(proplists:get_value(<<"cycleMode">>, Qs, <<>>)),
            case lists:member(CycleMode, ?CYCLE_MODE_LIST) of
                true -> 
                    WeekList = proplists:get_value(<<"weekList">>, Qs),
                    TimeList = get_time(Qs),
                    {TaskId, StrategyType, ValidityStart, ValidityEnd, CycleMode, WeekList, 
                     TimeList, ReissuedType, DataList};
                false ->
                    throw("not match cycleMode field")
            end;
        _ ->
            throw("unknown strategy type")
    end.

get_lighting_data(Qs) ->
    case proplists:get_value(<<"lightingData">>, Qs) of
        undefined ->
            [];
        GatewayCmdListJson ->
            Fun = 
                fun(GatewayCmdJson) ->
                        Gateway = binary_to_list(proplists:get_value(<<"gateway">>, GatewayCmdJson, <<>>)),
                        GatewayType = binary_to_list(proplists:get_value(<<"gatewayType">>, GatewayCmdJson, <<>>)),
                        CmdData = binary_to_list(proplists:get_value(<<"cmdData">>, GatewayCmdJson, <<>>)),
                        {Gateway, GatewayType, CmdData}
                end,
            lists:map(Fun, GatewayCmdListJson)
    end.

get_central_air_data(Qs) ->
    case proplists:get_value(<<"centralAirData">>, Qs) of
        undefined ->
            [];
        CentralAirDataJsonList ->
            Fun =
                fun(CentralAirDataJson) ->
                        Meters = proplists:get_value(<<"meters">>, CentralAirDataJson, []),
                        CmdOptList = proplists:get_value(<<"cmdList">>, CentralAirDataJson, []),
                        {Meters, CmdOptList}
                end,
            lists:map(Fun, CentralAirDataJsonList)               
    end.


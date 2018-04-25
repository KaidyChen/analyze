-module(send_cmd_to_middleware).

-include("analyze.hrl").
-include("analyze_config.hrl").
-include("cmd_obj.hrl").
-include("print.hrl").

-export([send/0, send/1, send/2, get_result/1]).

-define(SUCCESS_STATUS, <<"200">>).

send() ->
    Cmd_obj = #cmd_obj{
                 eqpt_type = "0a0003ahup",
                 eqpt_id_code = "000601206444",
                 cmd_type = "managerecord",
                 cmd_id = "readrecord",
                 cmd_data = ""
                },
    send(Cmd_obj).

send(Cmd_obj) ->
    send(Cmd_obj, 100).

send(Cmd_obj, Cq_weight) ->
    if
        Cq_weight =< 20 -> 
            ?PRINT_MSG("cq =< 20"), 
            "cq =< 20";
        true -> 
            {ok, ConnectorClientRequestOpt} = app_util:env(connector_client_request),
            Timeout = proplists:get_value(timeout, ConnectorClientRequestOpt),
            Url = proplists:get_value(url, ConnectorClientRequestOpt),
            OldPriority = proplists:get_value(task_priority, ConnectorClientRequestOpt, 0),
            TaskPriority = gen_priority(Cmd_obj, OldPriority), 
            Cmd_json = cmd_obj_to_json(Cmd_obj, TaskPriority), 
            send_(Url, Cmd_json, Timeout)
    end.

gen_priority(#cmd_obj{cmd_type = "dsj"}, _OldPriority) ->
    random:uniform(10) + 10;
gen_priority(_, OldPriority) ->
    OldPriority.
    
cmd_obj_to_json(Cmd_obj, TaskPriority)->
    Order_no = analyze_get_order_no:order_no(),
    Partner = "6ccpug",
    Eqpt_pwd = "0DD1F70B5F3A0B95C22126872845114A",
    Eqpt_type = Cmd_obj#cmd_obj.eqpt_type,
    Eqpt_id_code = Cmd_obj#cmd_obj.eqpt_id_code,
    Cmd_type = Cmd_obj#cmd_obj.cmd_type,
    Cmd_id = Cmd_obj#cmd_obj.cmd_id,
    Cmd_data = Cmd_obj#cmd_obj.cmd_data,
    Cmd_json = lists:concat(["order_no=", Order_no, "&partner=", Partner,"&objs=[{",
    "\"eqpt_type\"", ":","\"", Eqpt_type, "\"", "," , "\"eqpt_id_code\"", ":", "\"", Eqpt_id_code, "\"", ",", 
    "\"eqpt_pwd\"" , ":", "\"", Eqpt_pwd, "\"", "," , "\"resdatatype\"" , ":" ,"\"" , "\"" ,"," ,
    "\"cmd_type\"" , ":" , "\"" , Cmd_type,"\"" , "," ,"\"cmd_id\"" , ":" , "\"",Cmd_id , "\"" , ",",
    "\"cmd_data\"" , ":" , "\"", Cmd_data , "\"","}]" , "&sign=FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF&priority=", 
                             integer_to_list(TaskPriority)]),
    Cmd_json.

send_(Url, ReqBody, Timeout) ->
    ReqHeaders = [{<<"Content-Type">>, <<"application/x-www-form-urlencoded">>}],
    Options = [{pool, default}, {connect_timeout, Timeout}],
    case hackney:request(post, Url, ReqHeaders, ReqBody, Options) of
        {ok, _, _, Ref} ->
            case hackney:body(Ref) of
                {ok, Body} -> {ok, binary_to_list(Body)};
                {error, {closed, _}} -> {error, closed};
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            ?ERROR("request: ~p~n", [Reason]),
            {error, Reason}
    end.
                
get_result(Return_data) ->
    try
        case string:tokens(Return_data, "&") of
            [JsonStr | _] ->
                ?PRINT("~s~n", [JsonStr]),
                FieldMap = jsx:decode(helper_util:to_iolist(JsonStr), [return_maps]),
                case maps:find(<<"result">>, FieldMap) of
                    {ok, ResultBits} ->
                        case maps:find(<<"status">>, FieldMap) of
                            {ok, ?SUCCESS_STATUS} ->
                                {ok, binary_to_list(ResultBits)};
                            {ok, _} ->
                                {false, binary_to_list(ResultBits)};
                            _ ->
                                false
                        end;
                    _ ->
                        false
                end;
            _ ->
                false
        end
    catch
        _:_ ->
            false
    end.


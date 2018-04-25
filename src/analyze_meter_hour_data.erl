-module(analyze_meter_hour_data).


-include("analyze.hrl").
-include("analyze_config.hrl").
-include("print.hrl").
-include("cmd_obj.hrl").

-define(MAX_HOUR, 36).
-define(CMD_TYPE, "dsj").
-define(CMD_ID, "zddjsjk").

-export([start/1, parse_result/1, save_hour_data/3, get_last_hour_data_datetime/2]).


get_last_hour_data_datetime(Datetime_now, Meter) ->
    case get_last_record_date_time(Datetime_now, Meter) of
        {ok, Datetime_last} ->
            Datetime_last;
        _ ->
            %% 取前一个小时
            ?HELP:addHour(Datetime_now, -1)
    end.
    
get_last_record_date_time({{Year, Month, _Day}, _Time} = Datetime_now, Meter) ->
    case get_last_record_of_hour_data(Year, Month, Meter) of
        {ok, Last_record_line} ->
            case string:tokens(Last_record_line, ?FS) of
                [Date_str, Time_str, _Electric_power_str] ->
                    Date_last = ?HELP:strToDate(Date_str),
                    Time_last = ?HELP:strToTime(Time_str),
                    Datetime_last = {Date_last, Time_last},                    
                    {ok, Datetime_last};
                _ -> 
                    {error, Last_record_line}
            end;
        {error, enoent} ->
            {error, enoent};
        {error, Reason} -> 
            {error, Reason}
    end.

start({Meter_type, Meter, Gateway, Cq_weight, Datetime_last, Electric_power, Datetime_now, Is_on_the_hour}) ->
    ?PRINT("Datetime_last:~p~n", [Datetime_last]),
    complement_hour_data(Meter_type, Meter, Gateway, Cq_weight, Datetime_last, Electric_power, Datetime_now, Is_on_the_hour).

%% 以逗号分割的数据
parse_result(Result_str_tmp) ->
    case string:tokens(Result_str_tmp, ",") of
        [Datetime_str, Electric_power_str, _] ->
            [Year_str, Month_str, Day_str, Hour_str, Minute_str] = [lists:sublist(Datetime_str, X, 2) || X <- lists:seq(1, length(Datetime_str), 2)],
            Date = {2000 + list_to_integer(Year_str), list_to_integer(Month_str), list_to_integer(Day_str)},
            Time = {list_to_integer(Hour_str), list_to_integer(Minute_str), 0},
            Electric_power = list_to_float(Electric_power_str),
            Datetime = {Date, Time},
            {ok, Datetime, Electric_power};
        _ ->
            {error, Result_str_tmp}
    end. 


%% 解析返回的结果
% parse_result(Result_str_tmp) ->
%     Func = fun                 
%         ($a) -> false;
%         (_)  -> true
%     end,
%     Result_str = lists:filter(Func, Result_str_tmp),
%     Datetime_str = lists:sublist(Result_str, 1, 10),
%     if
%         (length(Result_str) =/= 26) orelse (Datetime_str =:= "0000000000") ->
%             {error, datetime_error};
%         true ->
%             Result_list = [lists:sublist(Result_str, X, 2) || X <- lists:seq(1, length(Result_str), 2)],
%             [Minute_str, Hour_str, Day_str, Month_str, Year_str] = lists:sublist(Result_list, 5),
%             Date = {2000 + list_to_integer(Year_str), list_to_integer(Month_str), list_to_integer(Day_str)},
%             Time = {list_to_integer(Hour_str), list_to_integer(Minute_str), 0},
%             Electric_power_list = lists:flatten(lists:reverse(lists:sublist(Result_list, 6, 4))),
%             Electric_power_list_1 = lists:sublist(Electric_power_list, 1, 6),
%             %% 获取小数部分(1byte, 2个字符)
%             Electric_power_list_2 = lists:sublist(Electric_power_list, 7, 2),
%             %% 拼接成完整数据 "整数部分.小数部分"
%             Electric_power_str = lists:concat([Electric_power_list_1, ".", Electric_power_list_2]),
%             Electric_power = list_to_float(Electric_power_str),
%             Datetime = {Date, Time},
%             ?ERROR("Result_str:~p Datetime:~p Electric_power:~p", [Result_str, Datetime, Electric_power]),
%             {ok, Datetime, Electric_power}
%     end. 

save_hour_data(Datetime, Meter, Electric_power) ->
    case save_hour_data_(Datetime, Meter, Electric_power) of
        ok              -> ok;
        {error, Reason} -> ?ERROR("save_hour_data Meter:~p is error:~p~n", [Meter, file:format_error(Reason)])
    end.

complement_hour_data(Meter_type, Meter, Gateway, Cq_weight, Datetime_last, Electric_power, Datetime_now, Is_on_the_hour) ->
    Datetime_tmp = case Is_on_the_hour of
        false ->
            {Date, {Hour, _, _}} = Datetime_now,
            {Date, {Hour, 0, 1}};
        true ->
            Datetime_now
    end,
    if 
        Datetime_last < Datetime_tmp ->
            %% 间隔多少小时
            Hour_diff = ?HELP:how_hourly_interval(Datetime_last, Datetime_tmp),
            case {Hour_diff, Is_on_the_hour} of
                {0, _} ->
                    %% 相差不足一个小时，不用写入文件
                    ok;
                {1, true} ->
                    %% 相差一个小时，且当前是整点, 向自身发送信息
                    self() ! {hour_data_on_the_hour, Datetime_tmp, Electric_power};
                {1, false} ->
                    %% 相差一个小时，但是当前不是整点，需要补抄数据
                    complementary_data(1, Meter_type, Meter, Gateway, Cq_weight, Datetime_last, Datetime_tmp, Is_on_the_hour);
                {N, true} when (N < ?MAX_HOUR) ->
                    %% 相差多个个小时，且当前是整点，补抄后再将当前数据直接写入文件
                    complementary_data(N+1, Meter_type, Meter, Gateway, Cq_weight, Datetime_last, Datetime_tmp, Is_on_the_hour);
                {N, false} when (N < ?MAX_HOUR) ->
                    complementary_data(N, Meter_type, Meter, Gateway, Cq_weight, Datetime_last, Datetime_tmp, Is_on_the_hour);
                {N, _} when (N >= ?MAX_HOUR) ->
                    %% 相差多个个小时，且当前是整点，补抄后再将当前数据直接写入文件
                    complementary_data(?MAX_HOUR, Meter_type, Meter, Gateway, Cq_weight, Datetime_last, Datetime_tmp, Is_on_the_hour)
            end;
        true ->
            ok
    end.

complementary_data(N, Meter_type, Meter, Gateway, Cq_weight, Datetime_last, Datetime_now, Is_on_the_hour) when (N > 0) ->
    case analyze_gateway_util:get_gateway_pid_by_gateway(Gateway) of
        {ok, Pid} ->
            build_send_zddjsjk_cmd_obj(Pid, N, Meter_type, Meter, self());
        {error, Reason} ->
            ?ERROR("get_gateway_pid_by_gateway(~p) is error:~p~n", [Gateway, Reason])
    end.

build_send_zddjsjk_cmd_obj(Pid, N, Meter_type, Meter, Self) when (N > 0) ->
    Cmd_obj_list = [get_zddjsjk_cmd_obj(X, Meter_type, Meter) || X <- lists:reverse(lists:seq(1, N))],
    Pid ! {hour_data, Self, Meter_type, Meter, Cmd_obj_list};
build_send_zddjsjk_cmd_obj(_, _, _, _, _) ->
    ok.

get_zddjsjk_cmd_obj(N, Meter_type, Meter) ->
    #cmd_obj{
        eqpt_type = Meter_type, 
        eqpt_id_code = Meter, 
        cmd_type = ?CMD_TYPE, 
        cmd_id = ?CMD_ID, 
        cmd_data = integer_to_list(N)
    }.

save_hour_data_(_Datetime = {Date, Time}, Meter, Electric_power) ->
    {Year, Month, _Day} = Date,
    FilePath = get_hour_data_file_path(Year, Month, Meter) ,
    filelib:ensure_dir(FilePath),
    Datetime_str = ?HELP:getDateTimeStr(Date, Time),
    Electric_power_str = float_to_list(Electric_power, [{decimals, 2}, compact]),
    Append_content = string:join([Datetime_str, Electric_power_str], ?FS),
    case save_hour_data_to_file(FilePath, Append_content) of
        ok              -> ok;
        {error, Reason} -> {error, Reason}
    end.

get_last_record_of_hour_data(Year, Month, Meter) ->
    FilePath = get_hour_data_file_path(Year, Month, Meter),
    case file:read_file(FilePath) of
        {ok, Binary_1} -> {ok, get_last_record(Binary_1)};
        {error, enoent} -> 
            {Tmp_year, Tmp_month} = ?HELP:get_prev_month(Year, Month),
            Prev_month_filePath = get_hour_data_file_path(Tmp_year, Tmp_month, Meter),
            case file:read_file(Prev_month_filePath) of
                {ok, Binary_2} -> {ok, get_last_record(Binary_2)};
                {error, enoent} -> {error, enoent}; 
                {error, Reason_2} -> {error, Reason_2}  
            end;
        {error, Reason_1} -> {error, Reason_1}
    end.

get_last_record(Data_Bin) ->
    Data_List = binary_to_list(Data_Bin),
    DataLine_List = lists:sort(string:tokens(Data_List, ?NL)),
    lists:last(DataLine_List).


%% 获取整点数据文件路径
get_hour_data_file_path(Year, Month, Meter) ->
    ?HELP:get_year_month_file_path(Year, Month, Meter, ?HOURDATADIR).

%% 储存整点数据
%% save_hour_data_to_file(FilePath, Append_content) -> ok | {error, Reason}
save_hour_data_to_file(FilePath, Append_content) ->
    ?HELP:append_content(FilePath, Append_content).

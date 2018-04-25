-module (analyze_ac_used_data_of_month).

-include("analyze.hrl").
-include("analyze_config.hrl").
-include("print.hrl").
-include("cmd_obj.hrl").

-define(CMD_TYPE, "dsj").
-define(CMD_ID, "syyktsyxx").

-export([start/1, parse_result/1, get_used_data_last_year_month/3, is_can_write/2, save_hour_data/3]).

is_can_write({Last_year, Last_month}, {Data_year, Data_month}) when (Data_year =:= Last_year) andalso (Data_month =< 12) andalso (Data_month > Last_month) ->
    true;
is_can_write({Last_year, Last_month}, {Data_year, Data_month}) when (Data_year =:= Last_year+1) andalso (Data_month =< 12) ->
    true;
is_can_write(_, _) ->
    false.
    
start({Meter_type, Meter, Gateway, Cq_weight, Used_data_last_year_month = {Last_year, Last_month}, Report_time = {Date, _}}) ->
    Prev_year_month = ?HELP:get_prev_month(Date),
    case how_monthly_interval(Used_data_last_year_month, Prev_year_month) of
        0 -> ok;
        N when (N >= 1) andalso (N =< 12) -> 
            complementary_data(Meter_type, Meter, Gateway, Cq_weight, N);
        N when (N > 12) ->
            complementary_data(Meter_type, Meter, Gateway, Cq_weight, 12)
    end.

get_used_data_last_year_month(Datetime_now, Meter_type, Meter) ->
    {{Year, Month, _} = Date, _} = Datetime_now,
    % {Prev_year, Prev_month} = ?HELP:get_prev_month(Date),
    {Prev_year, Prev_month} = {2016, 9},
    case get_used_data_last_year_month_filepath(Year, Meter) of
        {ok, Filepath} ->
            case get_used_data_last_year_month_0(Filepath) of
                {ok, {Last_year, Last_month}} ->
                    {Last_year, Last_month};
                _ ->
                    {Prev_year, Prev_month}
            end;
        {error, Reason} ->
            {Prev_year, Prev_month}
    end.

%% 解析返回的结果
parse_result(Result_str_tmp) ->
    case string:tokens(Result_str_tmp, ",") of
        [Freezing_time_str, Used_ele_1_of_month_str, Used_ele_2_of_month_str, Used_amount_1_of_month_str, Used_amount_2_of_month_str, Used_time_1_of_month_str, Used_time_2_of_month_str] ->
            Year_data_str = string:sub_string(Freezing_time_str, 1, 2),
            Month_data_str = string:sub_string(Freezing_time_str, 3, 4),
            Year_data = list_to_integer(Year_data_str) + 2000,
            Month_data = list_to_integer(Month_data_str),
            {Prev_year, Prev_month} = ?HELP:get_prev_month(Year_data, Month_data),
            Data_item_list = [Used_ele_1_of_month_str, Used_ele_2_of_month_str, Used_amount_1_of_month_str, Used_amount_2_of_month_str, Used_time_1_of_month_str, Used_time_2_of_month_str],
            {ok, {Prev_year, Prev_month}, Data_item_list};
        _ ->
            {error, not_valid_data}
    end.

save_hour_data({Year, Month}, Meter, Data_item_list) ->
    Year_month_str = ?HELP:year_and_month_str(Year, Month),
    Filepath = analyze_util:get_used_data_of_month_filepath(Year, Meter),
    Content = string:join([Year_month_str | Data_item_list], ?FS),
    filelib:ensure_dir(Filepath),
    ?ERROR("~p~n", [Content]),
    save_hour_data_(Filepath, Content),
    ok.

save_hour_data_(Filepath, Append_content) ->
    ?HELP:append_content(Filepath, Append_content).

complementary_data(Meter_type, Meter, Gateway, Cq_weight, N) when (N > 0) ->
    case analyze_gateway_util:get_gateway_pid_by_gateway(Gateway) of
        {ok, Pid} ->
            build_send_cmd_obj(Pid, N, Meter_type, Meter, self());
        {error, Reason} ->
            ?ERROR("get_gateway_pid_by_gateway(~p) is error:~p~n", [Gateway, Reason])
    end.

build_send_cmd_obj(Pid, N, Meter_type, Meter, Self) when (N > 0) ->
    Cmd_obj_list = [get_cmd_obj(X, Meter_type, Meter) || X <- lists:reverse(lists:seq(1, N))],
    Pid ! {used_data_of_month, Self, Meter_type, Meter, Cmd_obj_list};
build_send_cmd_obj(_, _, _, _, _) ->
    ok.

get_cmd_obj(N, Meter_type, Meter) ->
    #cmd_obj{
        eqpt_type = Meter_type, 
        eqpt_id_code = Meter, 
        cmd_type = ?CMD_TYPE, 
        cmd_id = ?CMD_ID, 
        cmd_data = integer_to_list(N)
    }.

%% 间隔多少月
how_monthly_interval({Year_min, Month_min}, {Year_max, Month_max}) when (Year_max =:= Year_min) andalso (Month_max >= Month_min) ->
    Month_max - Month_min;
how_monthly_interval({Year_min, Month_min}, {Year_max, Month_max}) when(Year_max > Year_min) ->
    (Year_max - Year_min) * 12 + Month_max - Month_min;
how_monthly_interval(_, _) ->
    erlang:error(badarg).

get_used_data_last_year_month_filepath(Year, Meter) ->
    Filepath1 = analyze_util:get_used_data_of_month_filepath(Year, Meter),
    case filelib:file_size(Filepath1) > 0 of
        true ->
            {ok, Filepath1};
        false ->
            Filepath2 = analyze_util:get_used_data_of_month_filepath(Year-1, Meter),
            case filelib:file_size(Filepath2) > 0 of
                true ->
                    {ok, Filepath2};
                false ->
                    {error, enoent}
            end
    end.

get_used_data_last_year_month_0(Filepath) ->
    case file:read_file(Filepath) of
        {ok, Binary} ->
            Data_list = binary_to_list(Binary),
            Data_line_list = string:tokens(Data_list, ?NL),
            [Last_line | _] = lists:reverse(lists:sort(Data_line_list)),
            [Year_month_str, Used_ele_1_of_month_str, Used_ele_2_of_month_str, Used_amount_1_of_month_str, Used_amount_2_of_month_str, Used_time_1_of_month_str, Used_time_2_of_month_str] = string:tokens(Last_line, ?FS),
            [Year_str, Month_str] = string:tokens(Year_month_str, "-"),
            {ok, {list_to_integer(Year_str), list_to_integer(Month_str)}};
        {error, Reason} ->
            {error, Reason}
    end.

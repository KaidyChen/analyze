-module (analyze_avg_temp_of_hour).

-include("analyze.hrl").
-include("analyze_config.hrl").
-include("print.hrl").

-export([start/3]).

start(Meter_type, Meter, Last_report_time) ->
    Datamsg_filepath = analyze_report_util:get_meter_datamsg_filepath(Meter, Last_report_time),
    case file:read_file(Datamsg_filepath) of
        {ok, Data_bin} ->
            Data_bitstring = binary_to_list(Data_bin),
            Data_line_list = string:tokens(Data_bitstring, ?NL),
            Last_report_time_str = ?HELP:getDateTimeStr(Last_report_time),
            Date_and_hour_str = hd(string:tokens(Last_report_time_str, ":")),
            Pred = fun(Data_line) ->
                    lists:prefix(Date_and_hour_str, Data_line)
            end,
            Data_list_of_hour = lists:filter(Pred, Data_line_list),
            Temp_list = get_temp_list_of_hour(Meter_type, Data_list_of_hour),
            ?PRINT("Temp_list~p~n", [Temp_list]),
            case Temp_list of
                [] -> ok;
                _ ->
                    Avg_temp = lists:sum(Temp_list) / length(Temp_list), 
                    Avg_temp_str = ?HELP:float_to_decimal_str(Avg_temp, 1), % 取一位小数, 26.64 -> "26.6"
                    Begin_datetime_str = analyze_report_util:get_datetime_str(Meter_type, hd(Data_list_of_hour)),
                    End_datetime_str = analyze_report_util:get_datetime_str(Meter_type, hd(lists:reverse(Data_list_of_hour))),
                    Content = string:join([Begin_datetime_str, End_datetime_str, Avg_temp_str], ?FS),
                    {{Year, Month, _}, _} = Last_report_time,
                    Avg_temp_filepath = get_avg_temp_filepath(Year, Month, Meter),
                    case save_avg_temp(Avg_temp_filepath, Content) of
                        ok -> ok;
                        {error, Reason} -> ?ERROR("~p save_avg_temp is error:~p", [?MODULE, Reason])
                    end,
                    %% 更新48小时的平均温度
                    analyze_48hour_avg_temp:start(Meter, Content),
                    ok
            end;
        {error, Reason} -> ?ERROR("~p read_file(~p) is error:~p", [?MODULE, Datamsg_filepath, Reason])
    end.

get_temp_list_of_hour(Meter_type, Data_list_of_hour) ->
    Fun = fun(Data_line) ->
            Temp_str = analyze_report_util:get_temp(Meter_type, Data_line),
            try list_to_float(Temp_str) of
                _ ->
                    {true, list_to_float(Temp_str)}
            catch
                Class:Reason ->
                    false
            end
    end,
    lists:filtermap(Fun, Data_list_of_hour).

%% 获取小时平均温度的文件路径
get_avg_temp_filepath(Year, Month, Meter) ->
    ?HELP:get_year_month_file_path(Year, Month, Meter, ?AVG_TEMP_DIR).

%% 存储数据完整率
%% save_integrity_rate(Filepath, Append_content) -> ok | {error, Reason}
save_avg_temp(Filepath, Append_content) ->
    filelib:ensure_dir(Filepath),
    ?HELP:append_content(Filepath, Append_content).
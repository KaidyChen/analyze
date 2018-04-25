-module (analyze_ac_conserve_ele_of_day).

-include("analyze.hrl").
-include("analyze_config.hrl").
-include("analyze_meter.hrl").
-include("print.hrl").

-export([start/0, start/1]).

start() ->
    start(?HELP:date()).

start(Today) ->
    Date = ?HELP:addDay(Today, -1), 
    Do_fun = fun
        (Meter_type) ->
            All_XX_meter = analyze_meter_util:select_meter_by_meter_type(Meter_type),
            lists:foreach(fun
                ([Meter]) ->
                    do_work(Meter_type, Meter, Date)
            end, All_XX_meter)
    end,
    [Do_fun(Meter_type) || Meter_type <- [?AC_TYPE]],
    ok.

do_work(Meter_type, Meter, Date) ->
    Datamsg_filepath = analyze_report_util:get_meter_datamsg_filepath(Meter, Date),
    case get_datamsg_list(Meter_type, Datamsg_filepath) of
        {ok, []} -> ok;
        {ok, Data_list} ->
            Used_ele = cal_used_ele(Data_list),
            Conserve_ele = cal_conserve_ele(Data_list, Used_ele),
            ?PRINT("Meter:~p Conserve_ele:~p~n", [Meter, Conserve_ele]),
            save_conserve_ele_of_day(Date, Meter, Conserve_ele),
            ok;
        {error, Reason} ->
            ?ERROR("get_datamsg_list(~p) is error: ~p", [Datamsg_filepath, Reason])
    end,
    ok.

get_datamsg_list(Meter_type, Datamsg_filepath) ->
    case file:read_file(Datamsg_filepath) of
        {ok, Data_bin} ->
            Data_bitstring = binary_to_list(Data_bin),
            Data_line_list = string:tokens(Data_bitstring, ?NL),
            Fun = fun
                (Data_line) ->
                    case analyze_report_util:get_temp(Meter_type, Data_line) of
                        "FFF.F" ->
                            false;
                        Temp ->
                            Datetime = analyze_report_util:get_datetime_tuple(Meter_type, Data_line),
                            Electric_power_float = analyze_report_util:get_ele(Meter_type, Data_line),
                            Active_power_float = analyze_report_util:get_active_power(Meter_type, Data_line),
                            Temp_float = list_to_float(Temp),
                            New_data_line = {Datetime, Electric_power_float, Active_power_float, Temp_float},
                            {true, New_data_line}
                    end                 
            end,
            New_data_line_list = lists:filtermap(Fun, lists:usort(Data_line_list)),
            {ok, New_data_line_list};
        {error, Reason} -> {error, Reason}
    end.

cal_used_ele(Data_list) ->
    [{_, First_ele, _, _} | _] = Data_list,
    [{_, Last_ele, _, _} | _] = lists:reverse(Data_list),
    Last_ele - First_ele.

cal_conserve_ele(Data_list, Used_ele) ->
    case get_time_start(Data_list) of
        {ok, Time_start} ->
            case cal_avgtemp_nonworking_and_working(Time_start, Data_list) of
                {ok, Avgtemp_nonworking, Avgtemp_working} when (Avgtemp_nonworking =< ?HEATING_TEMP) ->
                    %% 制暖模式
                    ?PRINT("Avgtemp_nonworking:~p Avgtemp_working:~p~n", [Avgtemp_nonworking, Avgtemp_working]),
                    case {Avgtemp_working < Avgtemp_nonworking, Avgtemp_working < ?THEORY_TEMP} of
                        %% 开机后的室温小于开机前的室温，不计算可节省电量
                        {true, _} ->    
                            0.0;
                        %% 为空调开机后处于一个合适的温度范围，合理用电
                        {false, true} ->
                            0.0;
                        %% 空调工作时的平均温度低于理论舒适温度，则为非合理使用空调，
                        %% 可节省电量为 真实的温度差与理论的温度差的差值比例乘以当日用电量
                        {false, false} ->
                            % Avgtemp_working > Avgtemp_nonworking
                            Real_delta_temp = erlang:abs(Avgtemp_working - Avgtemp_nonworking), 
                            % 表示制暖时理论上最多只开到26℃为宜，理论温度差=26℃-开机前室温
                            Theory_delta_temp = erlang:abs(?THEORY_TEMP - Avgtemp_nonworking), 
                            ((Real_delta_temp-Theory_delta_temp)/Real_delta_temp)*Used_ele
                    end;

                {ok, Avgtemp_nonworking, Avgtemp_working} -> 
                    %% 制冷模式 
                    case {Avgtemp_nonworking < ?THEORY_TEMP, ?THEORY_TEMP =< Avgtemp_working} of
                        %% 表示当日气温适宜，可不开空调, 可节省电量即为当日用电量
                        {true, _} ->    
                            Used_ele;
                        %% 空调工作时的平均温度大于理论舒适温度，则为合理使用空调，可节省电量为0
                        {false, true} ->
                            0.0;
                        %% 空调工作时的平均温度低于理论舒适温度，则为非合理使用空调，
                        %% 可节省电量为 真实的温度差与理论的温度差的差值比例乘以当日用电量
                        {false, false} ->
                            Real_delta_temp = erlang:abs(Avgtemp_nonworking - Avgtemp_working),
                            Theory_delta_temp = erlang:abs(Avgtemp_nonworking - ?THEORY_TEMP),
                            ((Real_delta_temp-Theory_delta_temp)/Real_delta_temp)*Used_ele
                    end;
                {error, _} ->
                    0.0
            end;
        {error, _} ->
            0.0
    end.

cal_avgtemp_nonworking_and_working(Time_start, Data_list) ->
    Prev_a_hour_time = ?HELP:addHour(Time_start, -1),
    ?PRINT("Prev_a_hour_time:~p Time_start:~p~n", [Prev_a_hour_time, Time_start]),
    Fun = fun
        (Data, {Temp_nonworking_list_tmp, Temp_working_list_tmp}) ->
            {Datetime, Electric_power_float, Active_power_float, Temp_float} = Data,
            ?PRINT("Datetime:~p ~n", [Datetime]),
            case {Prev_a_hour_time =< Datetime, Datetime < Time_start} of
                {true, true} ->
                    {[Temp_float | Temp_nonworking_list_tmp], Temp_working_list_tmp};
                _ ->
                    case Temp_float > ?STARTING_UP_ACTIVE_POWER of
                        true ->
                            {Temp_nonworking_list_tmp, [Temp_float | Temp_working_list_tmp]};
                        false ->
                            {Temp_nonworking_list_tmp, Temp_working_list_tmp}
                    end
            end
    end,
    {Temp_nonworking_list, Temp_working_list} = lists:foldl(Fun, {[], []}, Data_list),
    ?PRINT("Temp_nonworking_list:~p Temp_working_list:~p~n", [Temp_nonworking_list, Temp_working_list]),
    case {Temp_nonworking_list, Temp_working_list}  of
        {[], _}->
            {error, temp_nonworking_list_is_null};
        {_, []} ->
            {error, temp_working_list_is_null};
        _ ->
            {ok, lists:sum(Temp_nonworking_list)/length(Temp_nonworking_list), lists:sum(Temp_working_list)/length(Temp_working_list)}
    end.

get_time_start([Data | T]) ->
    {Datetime, Electric_power_float, Active_power_float, Temp_float} = Data,
    case (Active_power_float > ?STARTING_UP_ACTIVE_POWER) of
        false -> get_time_start(T);
        true -> {ok, Datetime}
    end;
get_time_start([]) ->
    {error, nonstartingup}.

save_conserve_ele_of_day(Date, Meter, Conserve_ele) ->
    Conserve_ele_str = ?HELP:float_to_decimal_str(Conserve_ele, 2),
    Date_str = ?HELP:dateToStr(Date, "-"),
    New_content = string:join([Date_str, Conserve_ele_str], ?FS),
    Conserve_ele_of_day_filepath = analyze_util:get_conserve_ele_of_day_filepath(Date, Meter),
    filelib:ensure_dir(Conserve_ele_of_day_filepath),
    case append_to_file(Conserve_ele_of_day_filepath, New_content) of
        ok -> ok;
        {error, Reason} ->
            ?ERROR("append_to_file is error:~p", [Reason])
    end.

append_to_file(Filepath, New_content) ->
    case file:open(Filepath, [binary, append]) of
        {ok, Fd} ->
            io:fwrite(Fd, "~s~n", [New_content]),
            file:close(Fd),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.



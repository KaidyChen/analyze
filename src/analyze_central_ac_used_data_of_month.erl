-module (analyze_central_ac_used_data_of_month).

-include("analyze.hrl").
-include("analyze_config.hrl").
-include("print.hrl").
-include("cmd_obj.hrl").

-export([start/3]).

start({Year, Month}, Meter, Data_item_list) ->
    Prev_year_month = ?HELP:get_prev_month(Year, Month),
    case analyze_central_ac_frozen_data_of_month:get_frozen_data_of_year_month(Prev_year_month, Meter) of
        {ok, Data_item_list_prev_month} ->
            Year_month_str = ?HELP:year_and_month_str(Year, Month),
            Used_data_list = get_used_data(Data_item_list_prev_month, Data_item_list),
            save_hour_data({Year, Month}, Meter, Used_data_list);
        {error, Reason} ->
            ok
    end.

get_used_data(Data_item_list_prev_month, Data_item_list) ->
    Number_type_list_2 = transform_data_type(Data_item_list),
    Number_type_list_1 = transform_data_type(Data_item_list_prev_month),
    gen_used_data(Number_type_list_1, Number_type_list_2, []).

gen_used_data([Number1 | List1], [Number2 | List2], Used_data_list) when is_integer(Number2)->
    gen_used_data(List1, List2, [integer_to_list(Number2-Number1) | Used_data_list]);
gen_used_data([Number1 | List1], [Number2 | List2], Used_data_list) when is_float(Number2)->
    gen_used_data(List1, List2, [?HELP:float_to_decimal_str(Number2-Number1, 2) | Used_data_list]);
gen_used_data(_, _, List) ->
    lists:reverse(List).
    
transform_data_type(Data_item_list) ->
    [Date_str, Time_str, Low_speed_used_time_str, Medium_speed_used_time_str, High_speed_used_time_str, Low_speed_used_amount_str, Medium_speed_used_amount_str, High_speed_used_amount_str] = Data_item_list,
    [list_to_integer(Low_speed_used_time_str), list_to_integer(Medium_speed_used_time_str), list_to_integer(High_speed_used_time_str), list_to_float(Low_speed_used_amount_str), list_to_float(Medium_speed_used_amount_str),list_to_float(High_speed_used_amount_str)].

save_hour_data({Year, Month}, Meter, Used_data_list) ->
    Year_month_str = ?HELP:year_and_month_str(Year, Month),
    Filepath = analyze_util:get_used_data_of_month_filepath(Year, Meter),
    Content = string:join([Year_month_str | Used_data_list], ?FS),
    filelib:ensure_dir(Filepath),
    ?ERROR("~p~n", [Content]),
    save_hour_data_(Filepath, Content),
    ok.

save_hour_data_(Filepath, Append_content) ->
    ?HELP:append_content(Filepath, Append_content).




-module(analyze_48hour_avg_temp).

-include("print.hrl").
-include("analyze.hrl").
-include("analyze_config.hrl").

-define(AVGTEMP_48HOUR, "48hour_avgtemp").
-define(HOW_HOUR, 48).

-export([start/2, get_48hour_avgtemp_filepath/1]).

start(Meter, New_avg_temp_line) ->
    case get_48hour_avgtemp_list(Meter) of
        {ok, Data_point_list} ->
            case save_48hour_avgtemp_list(Meter, [New_avg_temp_line | Data_point_list]) of
                ok -> ok;
                {error, Reason1} ->
                    ?ERROR("~p save_48hour_avgtemp_list(~p) is error: ~p", [?MODULE, Meter, Reason1])   
            end;
        {error, Reason} ->
            ?ERROR("~p get_48hour_avgtemp_list(~p) is error: ~p", [?MODULE, Meter, Reason])
    end.

get_48hour_avgtemp_list(Meter) ->
    Filepath = get_48hour_avgtemp_filepath(Meter),
    case file:read_file(Filepath) of
        {ok, Data_Bin} ->
            Data_List = binary_to_list(Data_Bin),
            DataLine_List = string:tokens(Data_List, ?NL),
            Reverse_data_List = lists:reverse(DataLine_List),
            {ok, lists:sublist(Reverse_data_List, ?HOW_HOUR - 1)};
        {error, enoent} -> {ok, []};
        {error, Reason} -> {error, Reason}
    end.

get_48hour_avgtemp_filepath(Meter) ->
    SaveFileName = lists:concat([?AVGTEMP_48HOUR, ?SUFFIX]),
    filename:join([?METERDIR, Meter, SaveFileName]).

save_48hour_avgtemp_list(Meter, Data_point_list) ->
    Filepath = get_48hour_avgtemp_filepath(Meter),
    Content = string:join(lists:reverse(Data_point_list), ?NL),
    case file:open(Filepath, [write, binary]) of
        {ok, Fd} ->
            io:fwrite(Fd, "~s~n", [Content]),
            file:close(Fd),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.
    
-module(task_util).

-include("analyze.hrl").
-include("analyze_config.hrl").

-compile(export_all).

-define(READ_HOW_DATA_POINT, 30).
-define(REALTIME_DATA_POINT_FILENAME, "realtime_data_point").

%%--------------------------------------------------------------------------
%% 实时数据点
%%--------------------------------------------------------------------------

get_realtime_data_point_list(Meter) ->
    Filepath = get_realtime_data_point_filepath(Meter),
    case file:read_file(Filepath) of
        {ok, Data_Bin} ->
            Data_List = binary_to_list(Data_Bin),
            DataLine_List = string:tokens(Data_List, ?NL),
            Reverse_data_List = lists:reverse(DataLine_List),
            {ok, lists:sublist(Reverse_data_List, ?READ_HOW_DATA_POINT - 1)};
        {error, enoent} -> {ok, []};
        {error, Reason} -> {error, Reason}
    end.

save_realtime_data_point_list(Meter, Data_point_list) ->
    Filepath = get_realtime_data_point_filepath(Meter),
    Content = string:join(lists:reverse(Data_point_list), ?NL),
    case file:open(Filepath, [write, binary]) of
        {ok, Fd} ->
            io:fwrite(Fd, "~s~n", [Content]),
            file:close(Fd),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

get_realtime_data_point_filepath(Meter) ->
    FileName = lists:concat([?REALTIME_DATA_POINT_FILENAME, ?SUFFIX]),
    filename:join([?METERDIR, Meter, FileName]).


%%------------------------------------------------------------------------
%% 
%%------------------------------------------------------------------------

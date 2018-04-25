-module(analyze_meter_cycle_task).

-include("analyze.hrl").
-include("analyze_config.hrl").
-include("print.hrl").

-define(FILENAME, "cycle_tasks.txt").

-export([add_cycle_task/1, del_cycle_task/1, del_all_cycle_task/1, getFilePath/1]).

add_cycle_task(Data_field_str) ->
    %% 002014110119#XXXXXXXXXXXX
    case string:tokens(Data_field_str, "#") of
        [Meter, Task] ->
            add_task(Meter, Task);
        _ ->
            ?DEBUG("Cycle task is not match.~n", [])
    end,
    ok.

del_cycle_task(Data_field_str) ->
    %% 002014110119#XXXXXXXXXXXX
    case string:tokens(Data_field_str, "#") of
        [Meter, Task] ->
            del_task(Meter, Task);
        _ ->
            ?DEBUG("Cycle task is not match.~n", [])
    end.

del_all_cycle_task(Data_field_str) ->
    %% 002014110119
    Meter = Data_field_str,
    FilePath = getFilePath(Meter),
    ?ERROR("FilePath:~p~n", [FilePath]),
    case file:delete(FilePath) of
        ok ->
            ?PRINT("del_all_cycle_task:~p is ok.~n", [Meter]);
        {error, Reason} ->
            ?ERROR("del_all_cycle_task:~p is error:~p.~n", [Meter, Reason])
    end.

add_task(Meter, Task) ->
    FilePath = getFilePath(Meter),
    filelib:ensure_dir(FilePath),
    case append_to_task_file(FilePath, Task) of
        ok ->
            ?DEBUG("add cycle task:~p is ok.~n", [Task]);
        {error, Reason} ->
            ?DEBUG("delete cycle task:~p is error:~p.~n", [Task, Reason])
    end,
    ok.

del_task(Meter, Task) ->
    FilePath = getFilePath(Meter),
    case read_file(FilePath) of
        {ok, Data_Bin} ->
            Data_Str = binary_to_list(Data_Bin),
            DataLine_List = string:tokens(Data_Str, ?NL),
            New_DataLine_List = [X || X <- DataLine_List, Task =/= X],
            case New_DataLine_List of
                [] ->
                    file:delete(FilePath);
                _ ->
                    New_File_Content = string:join(New_DataLine_List, ?NL),
                    case update_task_file(FilePath, New_File_Content) of
                        ok ->
                            ?DEBUG("delete cycle task:~p is ok.~n", [Task]);
                        {error, Reason} ->
                            ?DEBUG("delete cycle task:~p is error:~p.~n", [Task, Reason])
                    end
            end;
        {error, _} ->   
            false
    end,
    ok.

getFilePath(Meter) ->
    FilePath = filename:join([?METERDIR, Meter, ?FILENAME]),
    FilePath.

append_to_task_file(FilePath, Content) ->
    write_to_file(FilePath, Content, [append, binary]).
    
update_task_file(FilePath, Content) ->
    write_to_file(FilePath, Content, [write, binary]).
    
write_to_file(FilePath, Content, Modes) ->
    case file:open(FilePath, Modes) of
        {ok, Fd} ->
            io:fwrite(Fd, "~s~n", [Content]),
            file:close(Fd),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

read_file(FilePath) ->
    case filelib:is_regular(FilePath) of
        true ->
            case file:read_file(FilePath) of
                {ok, Data_Bin} ->
                    {ok, Data_Bin};
                {error, Reason} ->
                    {error, Reason}
            end;       
        false ->
            {error, "not_a_regular_file"}
    end.

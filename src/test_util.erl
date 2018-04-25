-module(test_util).

-compile([export_all]).

read_line(File) ->
    case file:open(File, [read, raw]) of
        {ok, IoDevice} ->
            read_line_(IoDevice),
            ok;
        {error, Reason} ->
            io:format("~p~n", [Reason])
    end.

read_line_(IoDevice) ->
    case file:read_line(IoDevice) of
        {ok, Data} ->
            %io:format("Data:~p~n", [Data]),
            read_line_(IoDevice);
        eof ->
            io:format("eof~n"),
            ok;
        {error, Reason} ->
            io:format("~p~n", [Reason])
    end.

read_file(File) ->
    {ok, Binary} = file:read_file(File),
    handle_line(Binary, 0).

handle_line(<<$\n, Rest/binary>>, LineCount) ->
    handle_line(<<Rest/binary>>, LineCount+1);
handle_line(<<C, Rest/binary>>, LineCount) ->
    handle_line(<<Rest/binary>>, LineCount);
handle_line(<<>>, LineCount) ->
    io:format("~p~n", [LineCount]).

read_file1(File) ->
    {ok, Binary} = file:read_file(File),
    Len = 81,
    PieceSize = 200,
    AllLen = binary:referenced_byte_size(Binary),
    RecordCount = AllLen div 81,
    ProcessCount = RecordCount div PieceSize,
    LastPieceSize = RecordCount rem PieceSize,
        
    handle_binary(Binary, Len, PieceSize, ProcessCount-1),
    read_part(Binary, Len, LastPieceSize, ProcessCount),            
    
    io:format("~p~n", [RecordCount]),
    io:format("~p~n", [ProcessCount]),
    ok.


handle_binary(Binary, Len, PieceSize, ProcessNum) when (ProcessNum >= 0) ->
    spawn(?MODULE, read_part, [Binary, Len, PieceSize, ProcessNum]),
    handle_binary(Binary, Len, PieceSize, ProcessNum-1);
handle_binary(_, _, _, _) ->
    ok.

read_part(Binary, Len, PieceSize, ProcessNum) ->
    read_each_line(Binary, Len, PieceSize-1, ProcessNum).

read_each_line(Binary, Len, CurPiece, ProcessNum) when (CurPiece >= 0)->
    LineBin = binary:part(Binary, ProcessNum*CurPiece*Len, Len),
    io:format("[~p]  ~p~n", [ProcessNum, binary_to_list(LineBin)]),
    read_each_line(Binary, Len, CurPiece-1, ProcessNum);
read_each_line(_, _, CurPiece, ProcessNum) ->
    ok.

    
    
    

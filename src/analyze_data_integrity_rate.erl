-module(analyze_data_integrity_rate).

-include("print.hrl").
-include("analyze.hrl").
-include("analyze_config.hrl").

-export([start/1]).

start({Meter, Integrity_rate_start_time, Integrity_rate_end_time, Received, Should_be_received}) ->
    % ?PRINT("~s/~p/~p/~p/~p~n", [Meter, Integrity_rate_start_time, Integrity_rate_end_time, Received, Should_be_received]),
    {Date, Time} = Integrity_rate_start_time,
    Start_datetime_str = ?HELP:getDateTimeStr(Date, Time),
    End_datetime_str = ?HELP:getDateTimeStr(Integrity_rate_end_time),
    Integrity_rate_str = ?HELP:float_to_decimal_str((Received / Should_be_received), 2),
    Append_content = string:join([Start_datetime_str, End_datetime_str, Integrity_rate_str], ?FS),
    {Year, Month, _Day} = Date,
    FilePath = analyze_util:get_integrity_rate_filepath(Year, Month, Meter),
    filelib:ensure_dir(FilePath),
    case save_integrity_rate(FilePath, Append_content) of
        ok              -> ok;
        {error, Reason} -> ?ERROR("save_integrity_rate is error:~p", [Reason])
    end,
    ok.

%% 存储数据完整率
%% save_integrity_rate(Filepath, Append_content) -> ok | {error, Reason}
save_integrity_rate(Filepath, Append_content) ->
    ?HELP:append_content(Filepath, Append_content).



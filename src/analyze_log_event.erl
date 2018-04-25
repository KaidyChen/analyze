-module(analyze_log_event).

-behaviour(gen_event).

-export([add_handler/0, delete_handler/0]).

-export([init/1,
         handle_event/2,
         handle_call/2,
         handle_info/2,
         terminate/2,
         code_change/3
]).

-record(state, {}).

-include("analyze.hrl").
-include("analyze_config.hrl").
-include("print.hrl").

%%===============================================================================
%% API
%%===============================================================================

add_handler() ->
    ?EVENT:add_handler(?MODULE, []).

delete_handler() ->
    ?EVENT:delete_handler(?MODULE, []).


%%===============================================================================
%% gen_event callback
%%===============================================================================

init([]) ->
    {ok, #state{}}.

handle_event({report_data, Msg_type, Data_list, Now_datetime}, State) ->
    report_data(Msg_type, Data_list, Now_datetime),
    {ok, State};
handle_event(_Event, State) ->
    {ok, State}.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%============================================================================================
%% Internal functions
%%============================================================================================

%%--------------------------------------------------------------------------------------------
%% 处理上报的报文
%%--------------------------------------------------------------------------------------------

report_data(Msg_type, Data_list, Now_datetime) ->
    Msg_filepath = get_report_data_filepath(Msg_type, Now_datetime),
    filelib:ensure_dir(Msg_filepath),
    %% 写入文件
    case save_report_data(Msg_filepath, Data_list, Now_datetime) of
        ok ->
            ?PRINT("write MsgFile:~p is ok~n", [Msg_filepath]);
        {error, Reason} ->
            ?ERROR("save_report_msg ~p is error:~p", [Msg_filepath, file:format_error(Reason)])
    end.

%% 获取上报的报文要写入的文件路径
%% Msg_type/Year/Month/Year-Month-Day + SUFFIX
get_report_data_filepath(Msg_type, Now_datetime) ->
    {Date, _Time} = Now_datetime,
    {Year, Month, Day} = Date,
    Date_str = ?HELP:dateToStr(Date, "-"),
    Filename = lists:concat([Date_str, ?SUFFIX]),
    Year_str = integer_to_list(Year),
    Month_str = integer_to_list(Month),
    filename:join([Msg_type, Year_str, Month_str, Filename]).

%% 时间戳加上电能数据块主动上报信息写入文件
%% Msg_filepath:文件路径
%% Data_list:上报的报文
save_report_data(Msg_filepath, Data_list, Now_datetime) ->
    analyze_report_util:save_datetime_and_Content(Msg_filepath, Data_list, Now_datetime).







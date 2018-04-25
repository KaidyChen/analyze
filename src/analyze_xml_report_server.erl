%%%-------------------------------------------------------------------  
%%% @author Administrator  
%%% @copyright (C) 2017, <COMPANY>  
%%% @doc  
%%%  
%%% @end  
%%% Created : 22. 七月 2017 14:58  
%%%-------------------------------------------------------------------  
-module(analyze_xml_report_server).  
-author("Administrator").  

-include("Energy.hrl").
-include("analyze.hrl").
-include("analyze_meter.hrl").
-include("analyze_config.hrl").
-include("print.hrl").

-behaviour(gen_server2).  


%% API  
-export([
         report_data/3,
         write_xsd_hrl_file/2,
         start_link/0
        ]).  

%% gen_server callbacks  
-export([
         init/1,  
         handle_call/3,  
         handle_cast/2,  
         handle_info/2,  
         terminate/2,  
         code_change/3
        ]).  

-define(SERVER, ?MODULE).  

-record(state, {
          energyModel,
          seq
         }).  


%%%===================================================================  
%%% API  
%%%===================================================================  

%%--------------------------------------------------------------------  
%% @doc  
%% Starts the server  
%%  
%% @end  
%%--------------------------------------------------------------------  
-spec(start_link() ->  
             {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).  
start_link() ->  
    gen_server2:start_link({local, ?SERVER}, ?MODULE, [], []).  

write_xsd_hrl_file(Xsd, Output) ->
    erlsom:write_xsd_hrl_file(Xsd, Output).

report_data(MeterId, DateTime, DataItemList) ->
    case app_util:env_proplists(xml_data_push, enable) of
        true ->            
            gen_server2:cast(?SERVER, {xml_report, MeterId, DateTime, DataItemList});
        _ ->
            ok
    end.

%%%===================================================================  
%%% gen_server callbacks  
%%%===================================================================  

%%--------------------------------------------------------------------  
%% @private  
%% @doc  
%% Initializes the server  
%%  
%% @spec init(Args) -> {ok, State} |  
%%                     {ok, State, Timeout} |  
%%                     ignore |  
%%                     {stop, Reason}  
%% @end  
%%--------------------------------------------------------------------  
-spec(init(Args :: term()) ->  
             {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |  
             {stop, Reason :: term()} | ignore).  
init([]) ->    
    EnergyXsd = energy_xsd(),
    {ok, EnergyModel} = erlsom:compile_xsd_file(EnergyXsd),    
    State = #state{
               energyModel = EnergyModel,
               seq = 1
              },
    {ok, State, 0}.  

%%--------------------------------------------------------------------  
%% @private  
%% @doc  
%% Handling call messages  
%%  
%% @end  
%%--------------------------------------------------------------------  
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},  
                  State :: #state{}) ->  
             {reply, Reply :: term(), NewState :: #state{}} |  
             {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |  
             {noreply, NewState :: #state{}} |  
             {noreply, NewState :: #state{}, timeout() | hibernate} |  
             {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |  
             {stop, Reason :: term(), NewState :: #state{}}).  
handle_call(_Request, _From, State) ->  
    {reply, ok, State}.  

%%--------------------------------------------------------------------  
%% @private  
%% @doc  
%% Handling cast messages  
%%  
%% @end  
%%--------------------------------------------------------------------  
-spec(handle_cast(Request :: term(), State :: #state{}) ->  
             {noreply, NewState :: #state{}} |  
             {noreply, NewState :: #state{}, timeout() | hibernate} |  
             {stop, Reason :: term(), NewState :: #state{}}).  
handle_cast({xml_report, MeterId, DateTime, DataItemList}, State) ->
    EnergyModel = State#state.energyModel,
    Seq = State#state.seq,
    AllDocument = gen_document(EnergyModel, Seq, MeterId, DateTime, DataItemList),
    ?PRINT("~p~n", [AllDocument]),
    
    SeqFun =
        fun(Seq) when Seq < 9999999999 ->
                Seq+1;
           (_Seq) ->
                1
        end,
    send_xml(AllDocument),
    {noreply, State#state{seq = SeqFun(Seq)}};
handle_cast(_Request, State) ->  
    {noreply, State}.  

%%--------------------------------------------------------------------  
%% @private  
%% @doc  
%% Handling all non call/cast messages  
%%  
%% @spec handle_info(Info, State) -> {noreply, State} |  
%%                                   {noreply, State, Timeout} |  
%%                                   {stop, Reason, State}  
%% @end  
%%--------------------------------------------------------------------  
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->  
             {noreply, NewState :: #state{}} |  
             {noreply, NewState :: #state{}, timeout() | hibernate} |  
             {stop, Reason :: term(), NewState :: #state{}}).  
handle_info(_Info, State) ->  
    {noreply, State}.  

%%--------------------------------------------------------------------  
%% @private  
%% @doc  
%% This function is called by a gen_server when it is about to  
%% terminate. It should be the opposite of Module:init/1 and do any  
%% necessary cleaning up. When it returns, the gen_server terminates  
%% with Reason. The return value is ignored.  
%%  
%% @spec terminate(Reason, State) -> void()  
%% @end  
%%--------------------------------------------------------------------  
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),  
                State :: #state{}) -> term()).  
terminate(_Reason, _State) ->  
    ok.  

%%--------------------------------------------------------------------  
%% @private  
%% @doc  
%% Convert process state when code is changed  
%%  
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}  
%% @end  
%%--------------------------------------------------------------------  
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},  
                  Extra :: term()) ->  
             {ok, NewState :: #state{}} | {error, Reason :: term()}).  
code_change(_OldVsn, State, _Extra) ->  
    {ok, State}.  

%%%===================================================================  
%%% Internal functions  
%%%===================================================================  

energy_xsd() ->
    filename:join(codeDir(), ["rel/", "Energy.xsd"]).
codeDir() ->
    filename:dirname("").

gen_common(Building_id, Gateway_id, Type) ->
    #'root/common'{
       building_id = Building_id,
       gateway_id = Gateway_id,
       type = Type
      }.

gen_data({Operation, Sequence, Parser, Time, MeterList}) ->
    #'root/data'{
       	operation = Operation,
	sequence = Sequence,
	parser = Parser,
	time = Time,
	meter = MeterList
      }.

gen_meter(Id, FunctionList) ->
    #'root/data/meter'{
       id = Id,
       function = FunctionList
      }.

gen_function(Id, Coding, Error, Text) ->
    #'root/data/meter/function'{
       id = Id,
       coding = Coding,
       error = Error,
       '#text' = Text
      }.
 
get_meter_list(MeterId, DataItemList) ->
    Fun = 
        fun({Flag, Text}) ->
                case help:is_number(Text) of
                    false ->
                        false;
                    true ->
                        {Id, Coding} = get_id_and_coding(Flag),
                        Error = "0",
                        {true, gen_function(Id, Coding, Error, Text)}
                end
        end,
    FunctionList = lists:filtermap(Fun, DataItemList),
    [gen_meter(MeterId, FunctionList)].

gen_seq_str(Seq) ->
    lists:flatten(io_lib:format("~10..0w", [Seq])).

gen_document(EnergyModel, Seq, MeterId, DateTime, DataItemList) ->
    ?PRINT("Energy Model:~p~n", [EnergyModel]),
    Building_id = "320200E021",
    Gateway_id = "01",
    Type = "report",
    Common = gen_common(Building_id, Gateway_id, Type),
    Operation = Type,
    Sequence = gen_seq_str(Seq),
    Parser = "no",
    Time = help:datetime_str(DateTime),
    MeterList = get_meter_list(MeterId, DataItemList),
    Data = gen_data({Operation, Sequence, Parser, Time, MeterList}),
    Root = #root{
              common = Common,
              data = Data
             },
    ?PRINT("Energy Root:~p~n", [Root]),
    {ok, Document} = erlsom:write(Root, EnergyModel, [{output, binary}]),
    DocumentHeader = get_xml_header(),
    AllDocument = <<DocumentHeader/binary, Document/binary>>.

energy_xml(DataCenterId, DateStr) ->
    Prefix = Dir = lists:flatten(lists:concat([DataCenterId, DateStr])),
    FileName = lists:flatten(lists:concat([Prefix, "Energy.xml"])),
    filename:join(codeDir(), [Dir, "/", FileName]).

get_xml_header() ->
   <<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>">>.

send_xml(AllDocument) ->
    {Ip, Port} = get_xml_push_ip_and_port(),
    case gen_tcp:connect(Ip, Port, ?CONN_OPTS, ?SOCKET_TIMEOUT) of
        {ok, Socket} ->
            case gen_tcp:send(Socket, AllDocument) of
                ok ->
                    ?PRINT("send xml is ok~n", []),
                    ok;
                {error, Reason} ->
                    ?ERROR("Send xml is error:~p~n", [Reason]),
                    ok
            end,
            ok = gen_tcp:close(Socket),
            ok;
        {error, What} ->
            ?ERROR("Send xml Connector is error:~p~n", [What]),
            ok
    end.
                    
get_xml_push_ip_and_port() ->
    {ok, XMLPushOpt} = app_util:env(xml_data_push),
    proplists:get_value(connect, XMLPushOpt).

get_id_and_coding(?TOTAL_ACTIVE_POWER) ->
    {"01", "01D10"};
get_id_and_coding(?A_ACTIVE_POWER) ->
    {"02", "01D10"};
get_id_and_coding(?B_ACTIVE_POWER) ->
    {"03", "01D10"};
get_id_and_coding(?C_ACTIVE_POWER) ->
    {"04", "01D10"};
get_id_and_coding(?TOTAL_REACTIVE_POWER) ->
    {"05", "01D10"};
get_id_and_coding(?A_REACTIVE_POWER) ->
    {"06", "01D10"};
get_id_and_coding(?B_REACTIVE_POWER) ->
    {"07", "01D10"};
get_id_and_coding(?C_REACTIVE_POWER) ->
    {"08", "01D10"};
get_id_and_coding(?TOTAL_POWER_FACTOR) ->
    {"09", "01D10"};
get_id_and_coding(?A_POWER_FACTOR) ->
    {"10", "01D10"};
get_id_and_coding(?B_POWER_FACTOR) ->
    {"11", "01D10"};
get_id_and_coding(?C_POWER_FACTOR) ->
    {"12", "01D10"};
get_id_and_coding(?A_VOLTAGE) ->
    {"13", "01D10"};
get_id_and_coding(?B_VOLTAGE) ->
    {"14", "01D10"};
get_id_and_coding(?C_VOLTAGE) ->
    {"15", "01D10"};
get_id_and_coding(?A_CURRENT) ->
    {"16", "01D10"};
get_id_and_coding(?B_CURRENT) ->
    {"17", "01D10"};
get_id_and_coding(?C_CURRENT) ->
    {"18", "01D10"};
get_id_and_coding(?ZERO_CURRENT) ->
    {"19", "01D10"};
get_id_and_coding(?TOTAL_APPARENT_POWER) ->
    {"20", "01D10"};
get_id_and_coding(?A_APPARENT_POWER) ->
    {"21", "01D10"};
get_id_and_coding(?B_APPARENT_POWER) ->
    {"22", "01D10"};
get_id_and_coding(?C_APPARENT_POWER) ->
    {"23", "01D10"};
get_id_and_coding(?POSITIVE_ACTIVE_ELE) ->
    {"24", "01D10"};
get_id_and_coding(?RATE1_POSITIVE_ACTIVE_ELE) ->
    {"25", "01D10"};
get_id_and_coding(?RATE2_POSITIVE_ACTIVE_ELE) ->
    {"26", "01D10"};
get_id_and_coding(?RATE3_POSITIVE_ACTIVE_ELE) ->
    {"27", "01D10"};
get_id_and_coding(?RATE4_POSITIVE_ACTIVE_ELE) ->
    {"28", "01D10"};
get_id_and_coding(?INVERSE_ACTIVE_ELE) ->
    {"29", "01D10"};
get_id_and_coding(?RATE1_INVERSE_ACTIVE_ELE) ->
    {"30", "01D10"};
get_id_and_coding(?RATE2_INVERSE_ACTIVE_ELE) ->
    {"31", "01D10"};
get_id_and_coding(?RATE3_INVERSE_ACTIVE_ELE) ->
    {"32", "01D10"};
get_id_and_coding(?RATE4_INVERSE_ACTIVE_ELE) ->
    {"33", "01D10"};
get_id_and_coding(?TOTAL_WATER_CONSUMPTION) ->
    {"34", "02A10"}.


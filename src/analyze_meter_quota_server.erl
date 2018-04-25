-module (analyze_meter_quota_server).

-include("analyze.hrl").
-include("analyze_config.hrl").
-include("print.hrl").
-include("meter_quota.hrl").

-behavisor(gen_server).

%% API
-export([start_link/0]).

-export([add_quota/1, del_quota/1, update_quota/1, get_quota/2, get_quota_and_mode/2]).

%% Gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%==================================================================
%%% API
%%%==================================================================

%%-------------------------------------------------------------------
%% @doc
%% Start the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add_quota(Data_field_str) ->
    %% 99.9#0#building=*&meter=150721023750&eqpt_type=0a0001aa7k
    case string:tokens(Data_field_str, "#") of
        [Quota_str, Mode_str, Condition] ->
            Quota = try list_to_float(Quota_str) of
                Quota_tmp ->
                    Quota_tmp
            catch
                _:_ ->
                    float(list_to_integer(Quota_str))
            end,
            Mode = list_to_integer(Mode_str),
            add_quota_(Quota, Mode, Condition);
        _ ->
            ?ERROR("add_quota Data_field_str:~p dismatch~n", [Data_field_str])
    end.

del_quota(Data_field_str) ->
    case string:tokens(Data_field_str, "#") of
        [Meter_type, Meter] ->
            del_quota_(Meter_type, Meter);
        _ ->
            ?ERROR("del_quota Data_field_str:~p dismatch~n", [Data_field_str])
    end.

add_quota_(Quota, Mode, Condition) ->
    gen_server:cast(?SERVER, {add_quota, Quota, Mode, Condition}).

del_quota_(Meter_type, Meter) ->
    gen_server:cast(?SERVER, {del_quota, Meter_type, Meter}).

update_quota(Meter_quota) ->
    gen_server:cast(?SERVER, {update_quota, Meter_quota}).

%% 多个进程并发读
get_quota(Meter_type, Meter) ->
    analyze_meter_quota_store:lookup(Meter_type, Meter).

get_quota_and_mode(Meter_type, Meter) ->
    case analyze_meter_quota_store:lookup(Meter_type, Meter) of
        {ok, Meter_quota} ->
            #meter_quota{
                base_quantity = Base_quantity, 
                cur_quantity = Cur_quantity,  
                quota = Quota,          
                mode = Mode
            } = Meter_quota,
            Quota_str = ?HELP:float_to_decimal_str(Quota, 2),
            Residues_quantity_str = ?HELP:float_to_decimal_str(Quota - (Cur_quantity - Base_quantity), 2),
            Mode_str = integer_to_list(Mode),
            {ok, Quota_str, Residues_quantity_str, Mode_str};
        Other -> Other
    end.


%%%==================================================================
%%% gen_server callbacks
%%%==================================================================

%%-------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server
%%
%% @spec init(Args) -> {ok, State} | 
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%-------------------------------------------------------------------
init([]) ->
    State = #state{},
    {ok, State, 0}.

%%-------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) -> 
%%                                      {reply, Reply, State} | 
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc
%%-------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%-------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% 
%% @spec handle_cast(Msg, State) ->
%%                          {noreply, State} |
%%                          {noreply, State, Timeout} |
%%                          {stop, Reason, State}
%% @end
%%-------------------------------------------------------------------
handle_cast({add_quota, Quota, Mode, Condition}, State) ->
    add_quota_by_condition(Quota, Mode, Condition),
    {noreply, State};
handle_cast({del_quota, Meter_type, Meter}, State) ->
    analyze_meter_quota_store:delete(Meter_type, Meter),
    {noreply, State};
handle_cast({update_quota, Meter_quota}, State) ->
    analyze_meter_quota_store:insert(Meter_quota),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%-------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) ->
%%                          {noreply, State} |
%%                          {noreply, State, Timeout} |
%%                          {stop, Reason, State}
%% @end
%%-------------------------------------------------------------------
handle_info(timeout, State) ->
    analyze_meter_quota_store:load(),
    {noreply, State};

handle_info(_Info, State) ->
    
    {noreply, State}.

%%-------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. when it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%-------------------------------------------------------------------
terminate(_Reason, _State) ->
    analyze_meter_quota_store:close_table(),
    ok.

%%-------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% 
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%-------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

add_quota_by_condition(Quota, Mode, Condition_str) ->
    {M, C} = analyze_meter_field_store:get_meter_field_list(10000),
    Condition_field = analyze_util:parse_condition(Condition_str),
    add_quota_by_condition_({M, C}, Quota, Mode, Condition_field).

add_quota_by_condition_({[], _}, _, _, _) ->
    ok;
add_quota_by_condition_({M, C}, Quota, Mode, Condition_field) ->
    MatchConditionList = analyze_util:get_meter_field_list_by_condition(Condition_field, lists:flatten(M)),
    ?PRINT("MatchConditionList:~p~n", [MatchConditionList]),
    case MatchConditionList of
        [] -> ok;
        _ ->
            set_quota(MatchConditionList, Quota, Mode)
    end,
    {NewM, NewC} = analyze_meter_field_store:get_meter_field_list(C),
    ?PRINT("New ~p~n~p~n", [NewM, NewC]),
    add_quota_by_condition_({NewM, NewC}, Quota, Mode, Condition_field).
    

set_quota(Meter_field_list, Quota_float, Mode_int) ->
    Fun = 
        fun
            (Meter_field) ->
                {Meter_type, Meter} = analyze_meter_field:get_meter_type_and_meter_by_meter_field(Meter_field),
                ?PRINT("~p~n", [Meter]),
                case analyze_meter_util:get_running_pid(Meter_type, Meter) of
                    {ok, Pid} -> 
                        Pid ! {set_quota, Quota_float, Mode_int};
                    {error, Reason} ->
                        ?ERROR("analyze_meter_util:get_running_pid(~p, ~p) is error:~p~n", [Meter_type, Meter])
                end
    end,
    lists:foreach(Fun, Meter_field_list).


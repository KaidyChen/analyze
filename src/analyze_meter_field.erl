-module (analyze_meter_field).

-include("analyze.hrl").
-include("analyze_config.hrl").

-export([
         get_meter_field_keypos/0,
         get_match_meter_and_meter_type_meter_field_by_gateway/1,
         get_meter_field_by_meter_info/1,
         
         get_meter_type_and_meter_by_meter_field/1,
         get_gateway_by_meter_field/1,
         get_build_id_by_meter_field/1,
         get_master_label_by_meter_field/1,
         get_slave_label_by_meter_field/1,

         get_meter_field_data_line_list/1,

         update_build_id/2
        ]).

-define(METER_FIELD_FS, "#").

%% 设备的字段信息
-record(meter_field, {
          key,            % 主键{meter_type, meter}
          gateway,        % 表所挂的网关号
          build_id,       % 建筑id
          master_label,   % 主标签
          slave_label     % 从标签
         }).

get_meter_field_keypos() ->
    #meter_field.key.

get_match_meter_and_meter_type_meter_field_by_gateway(Gateway) ->
    #meter_field{
       key = {'$2', '$1'},          
       gateway = Gateway,    
       _ = '_' 
      }.

get_meter_field_by_meter_info(Meter_info_tmp) ->
    Meter_info = ?HELP:strip([$\n, $\r, $ ], Meter_info_tmp),
    case string:tokens(Meter_info, ?METER_FIELD_FS) of
        [Meter_type, Meter, Gateway, Build_id, Master_label, Slave_label] ->
            {ok, build_meter_field({Meter_type, Meter, Gateway, Build_id, Master_label, Slave_label})};
        [Meter_type, Meter, Gateway, Build_id, Master_label] ->
            {ok, build_meter_field({Meter_type, Meter, Gateway, Build_id, Master_label, ?DEFAULT_SLAVE_LABEL})};
        _ ->
            {error, parse_error}
    end.

get_meter_field_data_line_list(MeterFieldList) ->
    Fun = 
        fun(MeterField) ->
                #meter_field{
                   key = {Meter_type, Meter},
                   gateway = Gateway,
                   build_id = Build_id,
                   master_label = Master_label,
                   slave_label = Slave_label
                  } = MeterField,
                string:join([Meter_type, Meter, Gateway, Build_id, Master_label, Slave_label], ?METER_FIELD_FS)
        end,
    string:join(lists:map(Fun, MeterFieldList), ?NL).                

%% 构建表信息字段
build_meter_field({Meter_type, Meter, Gateway, Build_id, Master_label, Slave_label}) ->
    #meter_field{
       key = {Meter_type, Meter},
       gateway = Gateway,          
       build_id = Build_id,           
       master_label = Master_label,
       slave_label = Slave_label
    }.



get_meter_type_and_meter_by_meter_field(Meter_field) when is_record(Meter_field, meter_field) ->
    Meter_field#meter_field.key.

get_gateway_by_meter_field(Meter_field) when is_record(Meter_field, meter_field) ->
    Meter_field#meter_field.gateway.

get_build_id_by_meter_field(Meter_field) when is_record(Meter_field, meter_field) ->
    Meter_field#meter_field.build_id.

get_master_label_by_meter_field(Meter_field) when is_record(Meter_field, meter_field) ->
    Meter_field#meter_field.master_label.

get_slave_label_by_meter_field(Meter_field) when is_record(Meter_field, meter_field) ->
    Meter_field#meter_field.slave_label.

update_build_id(Meter_field, Build_id) ->
    Meter_field#meter_field{build_id =  Build_id}.




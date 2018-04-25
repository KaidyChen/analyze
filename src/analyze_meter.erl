-module(analyze_meter).

-include("analyze.hrl").

-export([start_link/1]).

start_link(Meter_field) ->    
    {Meter_type, Meter} = analyze_meter_field:get_meter_type_and_meter_by_meter_field(Meter_field),
    case get_module_name_of_type(Meter_type) of
        {ok, Child_module} ->
            gen_server:start_link(Child_module, [Meter_field], []);
        {error, Reason} -> 
            {error, Reason}
    end.
    
%% 根据表类型获取对应的模块名
get_module_name_of_type(Meter_type) ->
    case lists:keyfind(Meter_type, 1, ?METER_TYPE_TO_MODULE)  of
        {Meter_type, Module_name} -> 
            case is_loaded(Module_name) of
                true ->
                    {ok, Module_name};
                {error, What} ->
                    {error, What}
            end;
        false -> 
            {error, not_found_module}
    end.

%% 模块是否加载
is_loaded(Module) ->
    case code:is_loaded(Module) of
        false ->
            case code:load_file(Module) of
                {module, Module} -> true;
                {error, What} -> {error, What}
            end;
        {file, _} ->
            true
    end.



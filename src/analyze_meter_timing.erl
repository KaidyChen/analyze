-module(analyze_meter_timing).

-export([start/4]).

-include("analyze.hrl").
-include("analyze_config.hrl").
-include("print.hrl").
-include("cmd_obj.hrl").

start(Meter_type, Meter, Cq_weight, Now)  ->
    Cmd_obj = #cmd_obj{
        eqpt_type = Meter_type, 
        eqpt_id_code = Meter, 
        cmd_type = "xsj", 
        cmd_id = "time", 
        cmd_data = ""
    },
    send_cmd_to_middleware:send(Cmd_obj, Cq_weight),
    ok.

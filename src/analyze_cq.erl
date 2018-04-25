-module(analyze_cq).

-export([cal_cq_weight/1]).

cal_cq_weight(Cq_list) ->
    Weight_list = [35, 25, 20, 10, 5, 5],
    cal_cq_weight_(Cq_list, Weight_list, 0).

cal_cq_weight_([Cq | Remain_cq_list], [Weight | Remain_weight_list], Weight_sum) ->
    case Cq of
        true ->
            cal_cq_weight_(Remain_cq_list, Remain_weight_list, Weight_sum + Weight);
        false ->
            cal_cq_weight_(Remain_cq_list, Remain_weight_list, Weight_sum)
    end;
cal_cq_weight_([], [], Weight_sum) ->
    Weight_sum.

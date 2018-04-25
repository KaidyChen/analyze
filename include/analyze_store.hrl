-record(lighting_strategy_rd, {
          task_id,
          strategy_type,
          validity_start,
          validity_end,
          cycle_mode,
          week_list,
          time_list,
          reissued_type,
          data_list
         }).

-define(TIMER_TYPE, "timer").
-define(TOUCH_TYPE, "touch").
-define(ONLY_ONCE, "onlyOnce").
-define(WEEK_LIST, "weekList").
-define(WORK_DAY, "workday").
-define(NOT_WORK_DAY, "notWorkday").

-define(CYCLE_MODE_LIST, 
        [
         ?ONLY_ONCE, 
         ?WEEK_LIST, 
         ?WORK_DAY, 
         ?NOT_WORK_DAY
        ]).

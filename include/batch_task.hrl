-record(batch_task, {
        task_id,
        task_status,
        datetime_start,
        datetime_end,
        operation_type,
        operation_argv,
        condition
    }).

-record(task_obj, {
        task_id,
        meter,
        meter_type,
        operation_type,
        operation_argv
    }).

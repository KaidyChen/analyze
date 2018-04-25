-record(time_table, {
        key,        %% 主键
        room_id,    %% 房间号
        type_and_labels_of_meter,   %% 设备类型及标签
        level,      % 课表级别， 0是常规课表，1是临时课表，在有效期重叠内，以级别1为高级别，执行课表1
        validity_date_start,        %% 有效期的起始日期
        validity_date_end,          %% 有效期的结束日期
        holiday_mode,               %% 节假日是否有效模式
        time_list                   %% 时段表，包含星期以及对应的时间段列表
    }).
-record(meter_quota, {
    key,            % 键值（{表类型, 表号}）
    base_quantity,  % 基础量
    quota,          % 定额值
    mode,           % 报警模式
    cur_quantity,   % 当前量
    warn_time_1,    % 报警次数1 接近定额值时报警
    warn_time_2     % 报警次数2 到达定额值时报警
}).
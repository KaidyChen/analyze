%%===========================================================
%% Print msg macro
%%===========================================================

%% DEBUG
-ifdef(NODEBUG).

-define(PRINT(_Format, _Args), ok).
-define(PRINT_MSG(_Msg), ok).
-define(DEBUG(_Format, _Args), ok).
-define(DEBUG_MSG(_Msg), ok).

-else.

-define(PRINT(Format, Args), io:format("{~p:~p}:" Format, [?MODULE, ?LINE] ++ Args)).
-define(PRINT_MSG(Msg), io:format("{~p:~p}:" Msg, [?MODULE, ?LINE])).
-define(DEBUG(Format, Args), io:format("{~p:~p}:" Format, [?MODULE, ?LINE] ++ Args)).
-define(DEBUG_MSG(Msg), io:format("{~p:~p}:" Msg, [?MODULE, ?LINE])).

-endif.

-define(INFO(Format, Args), lager:info("{~p:~p}:" Format, [?MODULE, ?LINE] ++ Args)).
-define(WARNING(Format, Args), lager:warning("{~p:~p}:" Format, [?MODULE, ?LINE] ++ Args)).
-define(ERROR(Format, Args), lager:error("{~p:~p}:" Format, [?MODULE, ?LINE] ++ Args)).

-define(INFO_MSG(Msg), lager:info("{~p:~p}:" Msg, [?MODULE, ?LINE])).
-define(WARNING_MSG(Msg), lager:warning("{~p:~p}:" Msg, [?MODULE, ?LINE])).
-define(ERROR_MSG(Msg), lager:error("{~p:~p}:" Msg, [?MODULE, ?LINE])).

%%===========================================================
%% End print msg macro
%%===========================================================

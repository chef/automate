-module(chef_log).

-compile({parse_transform, lager_transform}).

-export([
         log/2,
         log/3,
         log_item/3,
         log_item/4
        ]).

-export([
         debug/1,
         debug/2,
         info/1,
         info/2,
         notice/1,
         notice/2,
         warning/1,
         warning/2,
         error/1,
         error/2,
         critical/1,
         critical/2,
         alert/1,
         alert/2,
         emergency/1,
         emergency/2,
         failed_call/4
        ]).

-type log_level() :: lager:log_level(). %% debug | info | notice | warning | error | critical | alert | emergency.

-spec log(Level, Msg) -> _ when
    Level :: log_level(),
    Msg :: string().
log(Level, Msg) ->
    log(Level, Msg, []).

-spec log(Level, MsgFormat, FormatArgs) -> _ when
    Level :: log_level(),
    MsgFormat :: string(),
    FormatArgs :: list().
log(Level, MsgFormat, FormatArgs) ->
    lager:log(Level, self(), MsgFormat, FormatArgs).

-spec log_item(Level, Item, Msg) -> _ when
    Level :: log_level(),
    Item :: tuple() | list(tuple()),
    Msg :: string().
log_item(Level, Item, Msg) ->
    log_item(Level, Item, Msg, []).

-spec log_item(Level, Item, MsgFormat, FormatArgs) -> _ when
    Level :: log_level(),
    Item :: tuple() | list(tuple()),
    MsgFormat :: string(),
    FormatArgs :: list().
log_item(Level, [Item], MsgFormat, FormatArgs) ->
    log_item(Level, Item, MsgFormat, FormatArgs);
log_item(Level, Item, MsgFormat, FormatArgs) ->
    RecName = element(1, Item),
    RecId = element(2, Item),
    Fmt = "[~s ~p]: " ++ MsgFormat,
    log(Level, Fmt, [RecName, RecId | FormatArgs]).

%% bloated aliases, let's keep that separate from the rest
%% TODO: is there a better way to do that, apart from some parse_transform black magic?

-spec debug(string()) -> _.
debug(Msg) -> log(debug, Msg).
-spec debug(string(), list()) -> _.
debug(MsgFormat, FormatArgs) -> log(debug, MsgFormat, FormatArgs).
-spec info(string()) -> _.
info(Msg) -> log(info, Msg).
-spec info(string(), list()) -> _.
info(MsgFormat, FormatArgs) -> log(info, MsgFormat, FormatArgs).
-spec notice(string()) -> _.
notice(Msg) -> log(notice, Msg).
-spec notice(string(), list()) -> _.
notice(MsgFormat, FormatArgs) -> log(notice, MsgFormat, FormatArgs).
-spec warning(string()) -> _.
warning(Msg) -> log(warning, Msg).
-spec warning(string(), list()) -> _.
warning(MsgFormat, FormatArgs) -> log(warning, MsgFormat, FormatArgs).
-spec error(string()) -> _.
error(Msg) -> log(error, Msg).
-spec error(string(), list()) -> _.
error(MsgFormat, FormatArgs) -> log(error, MsgFormat, FormatArgs).
-spec critical(string()) -> _.
critical(Msg) -> log(critical, Msg).
-spec critical(string(), list()) -> _.
critical(MsgFormat, FormatArgs) -> log(critical, MsgFormat, FormatArgs).
-spec alert(string()) -> _.
alert(Msg) -> log(alert, Msg).
-spec alert(string(), list()) -> _.
alert(MsgFormat, FormatArgs) -> log(alert, MsgFormat, FormatArgs).
-spec emergency(string()) -> _.
emergency(Msg) -> log(emergency, Msg).
-spec emergency(string(), list()) -> _.
emergency(MsgFormat, FormatArgs) -> log(emergency, MsgFormat, FormatArgs).

%%
%% chef_log:failed_call(?MODULE, fun_name, [{reason, Why}, {extra_info, Value}]).
%%
-spec failed_call(atom(), atom(), list(), any()) -> _.
failed_call(Module, Function, Params, Why) ->
    log(error, "failed_call={~p:~p, ~p}; reason=~p", [Module, Function, Params, Why]).

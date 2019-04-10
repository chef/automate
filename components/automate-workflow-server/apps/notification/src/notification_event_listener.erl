-module(notification_event_listener).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec start_link(atom()) -> {ok, pid()}.
start_link(Notifier) ->
    gen_server:start_link({local, Notifier}, ?MODULE, {Notifier}, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

-spec init({atom()}) -> {ok, atom()}.
init({Notifier}) ->
    deliv_event:subscribe(notification_notifier:event_subscriptions()),
    {ok, Notifier}.

handle_call(_Request, _From, Notifier) ->
    {reply, ok, Notifier}.

handle_cast(_Msg, Notifier) ->
    {noreply, Notifier}.

handle_info({_Pid, EventType, EventBody}, Notifier) ->
    notification_notifier:notify(Notifier, EventType, EventBody),
    {noreply, Notifier}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, Notifier, _Extra) ->
    {ok, Notifier}.

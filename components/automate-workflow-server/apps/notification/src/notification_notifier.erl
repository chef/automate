-module(notification_notifier).

%% Behaviour for notification implementations.

%% To add a new notification:
%%   1. Add the event to notification_notifier:event_subscriptions
%%   2. Add a funhead to notification_notifier:change_and_event_type, returning
%%      {Change, event_type}
%%   3. Add functions notifier_*_content:event_type. If the corresponding notifier
%%      module (notification_notifier_*) does not support event_type, the return
%%      of notifier_*_content:event_type should be {error, no_content}.

%% To add a new notification notifier:
%%   1. Create a module notification_notifier_* implementing the callbacks
%%      defined in this behaviour
%%   2. Create a corresponding notification_*_content module for generating
%%      content for the notifications (this makes testing stupid easy)
%%   3. Add a new process to notification_sup for the new notifier

-include_lib("delivery/include/deliv_events.hrl").
-include("notification_types.hrl").

-callback notify(Config :: #notification_config{},
                 Event  :: notification_event(),
                 Change :: d_change()) -> ok.

-callback notification_type() -> notification_type().

-export([event_subscriptions/0,
         notify/3]).

-spec event_subscriptions() -> [term()].
event_subscriptions() ->
    AcceptanceEvent = {{stage, finished}, acceptance},
    VerifyPassedEvent = {{stage, finished}, verify},
    UnionFinishedEvent = {{stage, finished}, union},
    RehearsalFinishedEvent = {{stage, finished}, rehearsal},
    DeliveredFinishedEvent = {{stage, finished}, delivered},
    BuildFinishedEvent = {{stage, finished}, build},

    [
        AcceptanceEvent,
        BuildFinishedEvent,
        change_approved,
        change_delivered,
        comment_created,
        DeliveredFinishedEvent,
        RehearsalFinishedEvent,
        UnionFinishedEvent,
        VerifyPassedEvent
    ].

-spec notify(atom(), atom() | tuple(), d_change() | #stage_event{}) -> ok | {error, term()}.
notify(Notifier, Event, Msg) ->
    notify(Notifier, change_and_event_type(Event, Msg)).

%% PRIVATE

notify(_Notifier, {error, not_handled} = Error) ->
    %% We don't know how to process this event!!
    Error;
notify(Notifier, {EventPayload, EventType} = Event) ->
    Change1 = case Event of
                  {{Change, _Comment}, comment_created} ->
                      Change;
                  {Change, _EventType} ->
                      Change
              end,
    NotificationType = Notifier:notification_type(),
    Config = notification_config_db:fetch_enabled_config_by_type(NotificationType, Change1),
    handle_fetch(Config, Notifier, EventType, EventPayload).

handle_fetch({error, _Why} = Error, _Notifier, _EventType, _EventPayload) ->
    Error;
handle_fetch([], _Notifier, _EventType, _EventPayload) ->
    ok;
handle_fetch(Configs, Notifier, EventType, EventPayload) ->
    lists:foreach(
        fun(Config) ->
            Notifier:notify(Config, EventType, EventPayload)
        end,
        Configs
    ),
    ok.

%% Normalize event and event body. Returns a sanitized event atom of type
%% notification_event() type and the change associated with this event.
change_and_event_type({{stage, finished}, acceptance}, #stage_event{status = <<"passed">>,
                                                                    change = Change}) ->
    {Change, acceptance_passed};
change_and_event_type({{stage, finished}, acceptance}, #stage_event{status = <<"failed">>,
                                                                    change = Change}) ->
    {Change, acceptance_failed};
change_and_event_type({{stage, finished}, build}, #stage_event{status = <<"failed">>,
                                                               change = Change}) ->
    {Change, build_failed};
change_and_event_type(change_approved, Change) ->
    {Change, change_approved};
change_and_event_type(change_delivered, Change) ->
    {Change, change_delivered};
change_and_event_type(comment_created, {Comment, Change}) ->
    {{Change, Comment}, comment_created};
change_and_event_type({{stage, finished}, delivered}, #stage_event{status = <<"failed">>,
                                                                   change = Change}) ->
    {Change, delivered_failed};
change_and_event_type({{stage, finished}, delivered}, #stage_event{status = <<"passed">>,
                                                                   change = Change}) ->
    {Change, delivered_passed};
change_and_event_type({{stage, finished}, rehearsal}, #stage_event{status = <<"failed">>,
                                                                   change = Change}) ->
    {Change, rehearsal_failed};
change_and_event_type({{stage, finished}, union}, #stage_event{status = <<"failed">>,
                                                               change = Change}) ->
    {Change, union_failed};
change_and_event_type({{stage, finished}, verify}, #stage_event{status = <<"passed">>,
                                                                change = Change}) ->
    {Change, verify_passed};
change_and_event_type(_, _) ->
    {error, not_handled}.

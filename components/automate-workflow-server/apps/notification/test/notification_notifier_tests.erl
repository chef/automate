-module(notification_notifier_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_events.hrl").
-include("../src/notification_types.hrl").

-compile(export_all).

fixture_test_() ->
    hoax:fixture(?MODULE).

event_subscriptions_returns_a_list_of_all_the_event_subscriptions() ->
    AcceptanceEvent = {{stage, finished}, acceptance},
    VerifyPassedEvent = {{stage, finished}, verify},
    UnionFinishedEvent = {{stage, finished}, union},
    RehearsalFinishedEvent = {{stage, finished}, rehearsal},
    DeliveredFinishedEvent = {{stage, finished}, delivered},
    BuildFinishedEvent = {{stage, finished}, build},

    EventSubscriptions = [
                           AcceptanceEvent,
                           BuildFinishedEvent,
                           change_approved,
                           change_delivered,
                           comment_created,
                           DeliveredFinishedEvent,
                           RehearsalFinishedEvent,
                           UnionFinishedEvent,
                           VerifyPassedEvent
                         ],

    Result = notification_notifier:event_subscriptions(),
    ?assertEqual(EventSubscriptions, Result).

notify_returns_error_not_handled_when_event_is_not_handled() ->
    Notifier = notification_notifier_test,
    Event = {{stage, finished}, whoami},
    Msg = #stage_event{status = <<"passed">>, change = change},

    Result = notification_notifier:notify(Notifier, Event, Msg),

    ?assertEqual({error, not_handled}, Result),
    ?verifyAll.

notify_forwards_error_when_config_fetch_fails() ->
    Notifier = notification_notifier_test,
    NotifierType = test,

    hoax:mock(Notifier,
              ?expect(notification_type,
                      ?withArgs([]),
                      ?andReturn(NotifierType))),

    hoax:mock(notification_config_db,
              ?expect(fetch_enabled_config_by_type,
                      ?withArgs([NotifierType, change]),
                      ?andReturn({error, ohno}))),

    Event = {{stage, finished}, acceptance},
    Msg = #stage_event{status = <<"passed">>, change = change},

    Result = notification_notifier:notify(Notifier, Event, Msg),

    ?assertEqual({error, ohno}, Result),
    ?verifyAll.

notify_does_nothing_if_no_config_returned() ->
    Notifier = notification_notifier_test,
    NotifierType = test,

    hoax:mock(Notifier,
              ?expect(notification_type,
                      ?withArgs([]),
                      ?andReturn(NotifierType))),

    hoax:mock(notification_config_db,
              ?expect(fetch_enabled_config_by_type,
                      ?withArgs([NotifierType, change]),
                      ?andReturn([]))),

    Event = {{stage, finished}, acceptance},
    Msg = #stage_event{status = <<"passed">>, change = change},

    Result = notification_notifier:notify(Notifier, Event, Msg),

    ?assertEqual(ok, Result),
    ?verifyAll.

notify_acceptance_passed_notifies_acceptance_passed() ->
    Notifier = notification_notifier_test,
    NotifierType = test,

    hoax:mock(notification_config_db,
              ?expect(fetch_enabled_config_by_type,
                      ?withArgs([NotifierType, change]),
                      ?andReturn([config]))),

    hoax:mock(Notifier, [
              ?expect(notification_type,
                      ?withArgs([]),
                      ?andReturn(NotifierType)),
              ?expect(notify,
                      ?withArgs([config, acceptance_passed, change]),
                      ?andReturn(ok))
    ]),

    Event = {{stage, finished}, acceptance},
    Msg = #stage_event{status = <<"passed">>, change = change},

    Result = notification_notifier:notify(Notifier, Event, Msg),

    ?assertEqual(ok, Result),
    ?verifyAll.


notify_acceptance_failed_notifies_acceptance_failed() ->
    Notifier = notification_notifier_test,
    NotifierType = test,

    hoax:mock(notification_config_db,
              ?expect(fetch_enabled_config_by_type,
                      ?withArgs([NotifierType, change]),
                      ?andReturn([config]))),

    hoax:mock(Notifier, [
              ?expect(notification_type,
                      ?withArgs([]),
                      ?andReturn(NotifierType)),
              ?expect(notify,
                      ?withArgs([config, acceptance_failed, change]),
                      ?andReturn(ok))
    ]),

    Event = {{stage, finished}, acceptance},
    Msg = #stage_event{status = <<"failed">>, change = change},

    Result = notification_notifier:notify(Notifier, Event, Msg),

    ?assertEqual(ok, Result),
    ?verifyAll.

notify_change_approved_notifies_change_approved() ->
    Notifier = notification_notifier_test,
    NotifierType = test,

    hoax:mock(notification_config_db,
              ?expect(fetch_enabled_config_by_type,
                      ?withArgs([NotifierType, change]),
                      ?andReturn([config]))),

    hoax:mock(Notifier, [
              ?expect(notification_type,
                      ?withArgs([]),
                      ?andReturn(NotifierType)),
              ?expect(notify,
                      ?withArgs([config, change_approved, change]),
                      ?andReturn(ok))
    ]),

    Event = change_approved,
    Msg = change,

    Result = notification_notifier:notify(Notifier, Event, Msg),

    ?assertEqual(ok, Result),
    ?verifyAll.

notify_change_delivered_notifies_change_delivered() ->
    Notifier = notification_notifier_test,
    NotifierType = test,

    hoax:mock(notification_config_db,
              ?expect(fetch_enabled_config_by_type,
                      ?withArgs([NotifierType, change]),
                      ?andReturn([config]))),

    hoax:mock(Notifier, [
              ?expect(notification_type,
                      ?withArgs([]),
                      ?andReturn(NotifierType)),
              ?expect(notify,
                      ?withArgs([config, change_delivered, change]),
                      ?andReturn(ok))
    ]),

    Event = change_delivered,
    Msg = change,

    Result = notification_notifier:notify(Notifier, Event, Msg),

    ?assertEqual(ok, Result),
    ?verifyAll.

notify_delivered_failed_notifies_delivered_failed() ->
    Notifier = notification_notifier_test,
    NotifierType = test,

    hoax:mock(notification_config_db,
              ?expect(fetch_enabled_config_by_type,
                      ?withArgs([NotifierType, change]),
                      ?andReturn([config]))),

    hoax:mock(Notifier, [
              ?expect(notification_type,
                      ?withArgs([]),
                      ?andReturn(NotifierType)),
              ?expect(notify,
                      ?withArgs([config, delivered_failed, change]),
                      ?andReturn(ok))
    ]),

    Event = {{stage, finished}, delivered},
    Msg = #stage_event{status = <<"failed">>, change = change},

    Result = notification_notifier:notify(Notifier, Event, Msg),

    ?assertEqual(ok, Result),
    ?verifyAll.

notify_delivered_passed_notifies_delivered_passed() ->
    Notifier = notification_notifier_test,
    NotifierType = test,

    hoax:mock(notification_config_db,
              ?expect(fetch_enabled_config_by_type,
                      ?withArgs([NotifierType, change]),
                      ?andReturn([config]))),

    hoax:mock(Notifier, [
              ?expect(notification_type,
                      ?withArgs([]),
                      ?andReturn(NotifierType)),
              ?expect(notify,
                      ?withArgs([config, delivered_passed, change]),
                      ?andReturn(ok))
    ]),

    Event = {{stage, finished}, delivered},
    Msg = #stage_event{status = <<"passed">>, change = change},

    Result = notification_notifier:notify(Notifier, Event, Msg),

    ?assertEqual(ok, Result),
    ?verifyAll.

notify_rehearsal_failed_notifies_rehearsal_failed() ->
    Notifier = notification_notifier_test,
    NotifierType = test,

    hoax:mock(notification_config_db,
              ?expect(fetch_enabled_config_by_type,
                      ?withArgs([NotifierType, change]),
                      ?andReturn([config]))),

    hoax:mock(Notifier, [
              ?expect(notification_type,
                      ?withArgs([]),
                      ?andReturn(NotifierType)),
              ?expect(notify,
                      ?withArgs([config, rehearsal_failed, change]),
                      ?andReturn(ok))
    ]),

    Event = {{stage, finished}, rehearsal},
    Msg = #stage_event{status = <<"failed">>, change = change},

    Result = notification_notifier:notify(Notifier, Event, Msg),

    ?assertEqual(ok, Result),
    ?verifyAll.

notify_union_failed_notifies_union_failed() ->
    Notifier = notification_notifier_test,
    NotifierType = test,

    hoax:mock(notification_config_db,
              ?expect(fetch_enabled_config_by_type,
                      ?withArgs([NotifierType, change]),
                      ?andReturn([config]))),

    hoax:mock(Notifier, [
              ?expect(notification_type,
                      ?withArgs([]),
                      ?andReturn(NotifierType)),
              ?expect(notify,
                      ?withArgs([config, union_failed, change]),
                      ?andReturn(ok))
    ]),

    Event = {{stage, finished}, union},
    Msg = #stage_event{status = <<"failed">>, change = change},

    Result = notification_notifier:notify(Notifier, Event, Msg),

    ?assertEqual(ok, Result),
    ?verifyAll.

notify_verify_passed_notifies_verify_passed() ->
    Notifier = notification_notifier_test,
    NotifierType = test,

    hoax:mock(notification_config_db,
              ?expect(fetch_enabled_config_by_type,
                      ?withArgs([NotifierType, change]),
                      ?andReturn([config]))),

    hoax:mock(Notifier, [
              ?expect(notification_type,
                      ?withArgs([]),
                      ?andReturn(NotifierType)),
              ?expect(notify,
                      ?withArgs([config, verify_passed, change]),
                      ?andReturn(ok))
    ]),

    Event = {{stage, finished}, verify},
    Msg = #stage_event{status = <<"passed">>, change = change},

    Result = notification_notifier:notify(Notifier, Event, Msg),

    ?assertEqual(ok, Result),
    ?verifyAll.

notify_comment_created_notifies_comment_created() ->
    Notifier = notification_notifier_test,
    NotifierType = test,

    hoax:mock(notification_config_db,
              ?expect(fetch_enabled_config_by_type,
              ?withArgs([NotifierType, some_change]),
              ?andReturn([config]))),

    hoax:mock(Notifier, [
              ?expect(notification_type,
                      ?withArgs([]),
                      ?andReturn(NotifierType)),
              ?expect(notify,
                      ?withArgs([config, comment_created, {some_change, some_comment}]),
                      ?andReturn(ok))
    ]),

    Event = comment_created,
    Msg =  {some_comment, some_change},
    Result = notification_notifier:notify(Notifier, Event, Msg),

    ?assertEqual(ok, Result),
    ?verifyAll.

notify_with_two_configurations_notifies_twice() ->
    Notifier = notification_notifier_test,
    NotifierType = test,

    hoax:mock(notification_config_db,
              ?expect(fetch_enabled_config_by_type,
                      ?withArgs([NotifierType, change]),
                      ?andReturn([config, config]))),

    %% Hoax does not yet support calling the same function twice with different args.
    hoax:mock(Notifier, [
              ?expect(notification_type,
                      ?withArgs([]),
                      ?andReturn(NotifierType)),
              ?expect(notify,
                      ?withArgs([config, verify_passed, change]),
                      ?andReturn(ok),
                      ?times(2))
    ]),

    Event = {{stage, finished}, verify},
    Msg = #stage_event{status = <<"passed">>, change = change},
    Result = notification_notifier:notify(Notifier, Event, Msg),

    ?assertEqual(ok, Result),
    ?verifyAll.

notify_build_failed_notifies_build_failed() ->
    Notifier = notification_notifier_test,
    NotifierType = test,

    hoax:mock(notification_config_db,
              ?expect(fetch_enabled_config_by_type,
                      ?withArgs([NotifierType, change]),
                      ?andReturn([config]))),

    hoax:mock(Notifier, [
              ?expect(notification_type,
                      ?withArgs([]),
                      ?andReturn(NotifierType)),
              ?expect(notify,
                      ?withArgs([config, build_failed, change]),
                      ?andReturn(ok))
    ]),

    Event = {{stage, finished}, build},
    Msg = #stage_event{status = <<"failed">>, change = change},

    Result = notification_notifier:notify(Notifier, Event, Msg),

    ?assertEqual(ok, Result),
    ?verifyAll.

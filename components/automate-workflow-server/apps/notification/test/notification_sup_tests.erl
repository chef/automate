-module(notification_sup_tests).

-include_lib("hoax/include/hoax.hrl").
-include("../src/notification_macros.hrl").

-compile(export_all).

start_link_calls_supervisor_test() ->
    hoax:test(fun() ->

        hoax:mock(supervisor,
                  ?expect(start_link,
                          ?withArgs([{local, notification_sup}, notification_sup, []]))),

        notification_sup:start_link(),

        ?verifyAll
    end).

init_returns_child_specs_with_procs_test() ->
    hoax:test(fun() ->
        Return = notification_sup:init([]),
        Expected = {ok, { {one_for_one, 5, 10}, [
            {
                notification_notifier_smtp,
                {notification_event_listener, start_link, [notification_notifier_smtp]},
                permanent,
                5000,
                worker,
                [notification_event_listener]
            },
            {
                notification_notifier_slack,
                {notification_event_listener, start_link, [notification_notifier_slack]},
                permanent,
                5000,
                worker,
                [notification_event_listener]
            }
        ]}},
        ?assertEqual(Return, Expected)
    end).

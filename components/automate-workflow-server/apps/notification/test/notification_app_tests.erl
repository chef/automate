-module(notification_app_tests).

-include_lib("hoax/include/hoax.hrl").

-compile(export_all).

fixture_test_() ->
  hoax:fixture(?MODULE).

start_calls_notification_sup_test() ->
    hoax:test(fun() ->
        hoax:mock(notification_sup,
                  ?expect(start_link,
                          ?withArgs([]),
                          ?times(1))),

        notification_app:start(start_type, start_args),

        ?verifyAll
    end).

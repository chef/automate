-module(insights_app_tests).

-include_lib("hoax/include/hoax.hrl").

-compile(export_all).

start_calls_insights_sup_test() ->
    hoax:test(fun() ->
        hoax:mock(insights_sup,
                  ?expect(start_link,
                          ?withArgs([]),
                          ?times(1))),

        insights_app:start(start_type, start_args),

        ?verifyAll
    end).

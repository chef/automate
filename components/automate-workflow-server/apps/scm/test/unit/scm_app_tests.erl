-module(scm_app_tests).

-include_lib("hoax/include/hoax.hrl").

-compile(export_all).

start_test_() ->
    hoax:fixture(?MODULE, "start_").

stop_test_() ->
    hoax:fixture(?MODULE, "stop_").

start_calls_start_link_on_scm_sup() ->
    hoax:mock(scm_sup,
              ?expect(start_link,
                      ?withArgs([]),
                      ?andReturn({ok, list_to_pid("<0.30.0>")}))),
    ?assertEqual({ok, list_to_pid("<0.30.0>")},
                 scm_app:start(start_type, start_args)),
    ?verifyAll.

stop_returns_ok() ->
    ?assertEqual(ok, scm_app:stop(state)).

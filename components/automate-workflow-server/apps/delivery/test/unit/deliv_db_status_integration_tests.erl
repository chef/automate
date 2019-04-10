-module(deliv_db_status_integration_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_types.hrl").

-compile([export_all]).

ping_fixture_test_() ->
    hoax:fixture(?MODULE, ping).

ping_with_db_up_returns_pong() ->
    error_logger:tty(false),
    eu_database:setup(),

    ?assertEqual(#status_metadata{ service = <<"postgres">>, status = pong}, deliv_db_status:ping(standalone)),
    ?verifyAll,

    eu_database:teardown(),
    error_logger:tty(true).

ping_with_db_down_returns_fail() ->
    hoax:mock(chef_log,
              ?expect(log,
              ?withArgs([error, "PING FAILED: ~p", '_']))),

    ?assertEqual(#status_metadata{service = <<"postgres">>, status = fail}, deliv_db_status:ping(standalone)),
    ?verifyAll.

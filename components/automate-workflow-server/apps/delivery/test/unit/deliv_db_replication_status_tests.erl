-module(deliv_db_replication_status_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_types.hrl").

-compile([export_all]).

ping_fixture_test_() ->
    hoax:fixture(?MODULE, ping).

ping_with_db_up_and_returns_replication_status_for_primary() ->
    hoax:mock(sqerl,
              ?expect(execute,
                      ?withArgs([<<"SELECT client_addr, pg_current_xlog_location FROM pg_stat_repl, pg_current_xlog_location()">>]),
                      ?andReturn({ok,[[{<<"client_addr">>,{192,168,33,13}},{<<"pg_current_xlog_location">>,<<"0/7027770">>}]]}))),

    Attributes = [{<<"standby_ip_address">>,<<"192.168.33.13">>},
                            {<<"pg_current_xlog_location">>, <<"0/7027770">>}],

    ?assertEqual({pong, Attributes}, deliv_db_replication_status:ping(primary)),
    ?verifyAll.

ping_with_db_up_and_returns_error_replication_status_for_primary() ->
    hoax:mock(sqerl,
              ?expect(execute,
                      ?withArgs([<<"SELECT client_addr, pg_current_xlog_location FROM pg_stat_repl, pg_current_xlog_location()">>]),
                      ?andReturn({error, reason}))),
    hoax:mock(chef_log,
              ?expect(log,
              ?withArgs([error, "Primary Replication Query failed: ~p", '_']))),

    Attributes = [{<<"replication">>, <<"fail">>}, {<<"description">>, <<"Replication is not running. Check your configuration.">>}],

    ?assertEqual({degraded, Attributes}, deliv_db_replication_status:ping(primary)),
    ?verifyAll.

ping_with_db_up_and_returns_error_when_replication_status_has_no_results() ->
    hoax:mock(sqerl,
              ?expect(execute,
                      ?withArgs([<<"SELECT client_addr, pg_current_xlog_location FROM pg_stat_repl, pg_current_xlog_location()">>]),
                      ?andReturn({ok, []}))),
    hoax:mock(chef_log,
              ?expect(log,
              ?withArgs([error, "Primary Replication Query failed: no results"]))),

    Attributes = [{<<"replication">>, <<"fail">>}, {<<"description">>, <<"Replication is not running. Check your configuration.">>}],

    ?assertEqual({degraded, Attributes}, deliv_db_replication_status:ping(primary)),
    ?verifyAll.


ping_with_db_up_and_returns_replication_status_for_secondary() ->
    hoax:mock(sqerl,
              ?expect(execute,
                      ?withArgs([<<"SELECT pg_last_xlog_receive_location()">>]),
                      ?andReturn({ok,[[{<<"pg_last_xlog_receive_location">>, <<"0/7028048">>}]]}))
                     ),

    Attributes = [{<<"pg_last_xlog_receive_location">>,<<"0/7028048">>}],

    ?assertEqual({pong, Attributes}, deliv_db_replication_status:ping(cold_standby)),
    ?verifyAll.

ping_with_db_up_and_returns_error_replication_status_for_secondary() ->
    hoax:mock(sqerl,
              ?expect(execute,
                      ?withArgs([<<"SELECT pg_last_xlog_receive_location()">>]),
                      ?andReturn({error, reason}))
                     ),
    hoax:mock(chef_log,
              ?expect(log,
              ?withArgs([error, "Secondary Location Query failed: ~p", '_']))),

    ?assertEqual({degraded, [{<<"replication">>, <<"fail">>}]}, deliv_db_replication_status:ping(cold_standby)),
    ?verifyAll.

ping_in_standalone_configuration_returns_empty_list() ->
    ?assertEqual({pong, []}, deliv_db_replication_status:ping(standalone)).

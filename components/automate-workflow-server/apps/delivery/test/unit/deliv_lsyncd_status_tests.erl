-module(deliv_lsyncd_status_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_types.hrl").

-compile([export_all]).

ping_fixture_test_() ->
    hoax:fixture(?MODULE, ping).

lsyncd_is_available_no_status_log_test_() ->
    hoax:parameterized_fixture(?MODULE, "lsyncd_is_available_no_status_log_", lsyncd_available_setup, teardown).

lsyncd_is_available_with_latency_test_() ->
    hoax:parameterized_fixture(?MODULE, "lsyncd_is_available_with_latency_", lsyncd_available_with_latency_setup, teardown).

lsyncd_is_available_with_multiple_latency_test_() ->
    hoax:parameterized_fixture(?MODULE, "lsyncd_is_available_with_multiple_latency_", lsyncd_available_with_multiple_latency_setup, teardown).

lsyncd_is_down_test_() ->
    hoax:parameterized_fixture(?MODULE, "lsyncd_is_down_", lsyncd_down_setup, teardown).

no_stat_file_test_() ->
    hoax:fixture(?MODULE, "no_stat_file_").

create_temp_file(Content) ->
    File = lib:nonl(os:cmd("mktemp lsyncd-XXXXXXXX.tmp")),
    file:write_file(File, Content),
    File.

lsyncd_available_with_latency_setup() ->
    StatFile = create_temp_file(<<"run\n">>),
    LatencyFile = create_temp_file(<<"There are 100 delays\n">>),
    [StatFile, LatencyFile].

lsyncd_available_with_multiple_latency_setup() ->
    StatFile = create_temp_file(<<"run\n">>),
    LatencyFile = create_temp_file(<<"There are 100 delays\nThere are 5 delays\n">>),
    [StatFile, LatencyFile].

lsyncd_available_setup() ->
    StatFile = create_temp_file(<<"run\n">>),
    [StatFile].

lsyncd_down_setup() ->
    StatFile = create_temp_file(<<"down\n">>),
    [StatFile].

teardown([StatFile|T]) ->
    file:delete(StatFile),
    teardown(T);
teardown([]) ->
    ok.

no_stat_file_on_secondary_should_return_not_running() ->
    hoax:mock(application,
               ?expect(get_env,
                       ?withArgs([delivery, lsyncd_stat_path]),
                       ?andReturn({ok, "no/such/path"}))),

    ?assertEqual(#status_metadata{service = <<"lsyncd">>, status = not_running},
                 deliv_lsyncd_status:ping(cold_standby)),
    ?verifyAll.

no_stat_file_on_primary_should_return_fail() ->
    hoax:mock(application,
               ?expect(get_env,
                       ?withArgs([delivery, lsyncd_stat_path]),
                       ?andReturn({ok, "no/such/path"}))),

    ?assertEqual(#status_metadata{service = <<"lsyncd">>, status = fail},
                 deliv_lsyncd_status:ping(primary)),
    ?verifyAll.

lsyncd_is_down_on_secondary_should_return_not_running([StatFile]) ->
    hoax:mock(application,
               ?expect(get_env,
                       ?withArgs([delivery, lsyncd_stat_path]),
                       ?andReturn({ok, StatFile}))),
    ?assertEqual(#status_metadata{service = <<"lsyncd">>, status = not_running},
                 deliv_lsyncd_status:ping(cold_standby)),
    ?verifyAll.

lsyncd_is_available_no_status_log_on_secondary_should_return_fail_and_description([StatFile]) ->
    hoax:mock(application,
               ?expect(get_env,
                       ?withArgs([delivery, lsyncd_stat_path]),
                       ?andReturn({ok, StatFile}))),

    ?assertEqual(#status_metadata{service = <<"lsyncd">>, status = pong, additional_attributes = [{<<"description">>, <<"Server configured as cold standby. Lsyncd should only be run on the primary.">>}]},
                 deliv_lsyncd_status:ping(cold_standby)),
    ?verifyAll.

lsyncd_is_available_no_status_log_on_primary_should_return_pong([StatFile]) ->
    hoax:mock(application,
              [
               ?expect(get_env,
                       ?withArgs([delivery, lsyncd_stat_path]),
                       ?andReturn({ok, StatFile})),
               ?expect(get_env,
                       ?withArgs([delivery, lsyncd_log_file_path]),
                       ?andReturn({ok, "no/such/file"}))
              ]),
    Expected = #status_metadata{service = <<"lsyncd">>, status = pong, additional_attributes=[{<<"latency">>, <<"Could not read status log.">>}]},
    ?assertEqual(Expected,
                 deliv_lsyncd_status:ping(primary)),
    ?verifyAll.

lsyncd_is_available_with_multiple_latency_on_primary_should_return_pong_with_latency([StatFile, LatencyFile]) ->
    hoax:mock(application,
              [
               ?expect(get_env,
                       ?withArgs([delivery, lsyncd_stat_path]),
                       ?andReturn({ok, StatFile})),
               ?expect(get_env,
                       ?withArgs([delivery, lsyncd_log_file_path]),
                       ?andReturn({ok, LatencyFile}))
              ]),
    Expected = #status_metadata{service = <<"lsyncd">>, status = pong, additional_attributes=[{<<"latency">>, <<"105">>}]},
    ?assertEqual(Expected,
                 deliv_lsyncd_status:ping(primary)),
    ?verifyAll.

lsyncd_is_available_with_latency_on_primary_should_return_pong_with_latency([StatFile, LatencyFile]) ->
    hoax:mock(application,
              [
               ?expect(get_env,
                       ?withArgs([delivery, lsyncd_stat_path]),
                       ?andReturn({ok, StatFile})),
               ?expect(get_env,
                       ?withArgs([delivery, lsyncd_log_file_path]),
                       ?andReturn({ok, LatencyFile}))
              ]),
    Expected = #status_metadata{service = <<"lsyncd">>, status = pong, additional_attributes=[{<<"latency">>, <<"100">>}]},
    ?assertEqual(Expected, deliv_lsyncd_status:ping(primary)),
    ?verifyAll.

lsyncd_is_down_on_primary_should_return_fail([StatFile]) ->
    hoax:mock(application,
              [
               ?expect(get_env,
                       ?withArgs([delivery, lsyncd_stat_path]),
                       ?andReturn({ok, StatFile}))
              ]),
    ?assertEqual(#status_metadata{service = <<"lsyncd">>, status = fail},
                 deliv_lsyncd_status:ping(primary)),
    ?verifyAll.

no_stat_file_on_primary_when_no_disaster_recovery_should_return_not_running() ->
    hoax:mock(application,
               ?expect(get_env,
                       ?withArgs([delivery, lsyncd_stat_path]),
                       ?andReturn({ok, "no/such/path"}))),

    ?assertEqual(#status_metadata{service = <<"lsyncd">>, status = not_running},
                 deliv_lsyncd_status:ping(standalone)),
    ?verifyAll.

lsyncd_is_available_on_primary_when_not_in_disaster_recovery_should_returns_pong_and_description([StatFile]) ->
    hoax:mock(application,
               ?expect(get_env,
                       ?withArgs([delivery, lsyncd_stat_path]),
                       ?andReturn({ok, StatFile}))),

    ?assertEqual(#status_metadata{service = <<"lsyncd">>, status = pong, additional_attributes = [{<<"description">>, <<"No backup server configured.">>}]},
                 deliv_lsyncd_status:ping(standalone)),
    ?verifyAll.

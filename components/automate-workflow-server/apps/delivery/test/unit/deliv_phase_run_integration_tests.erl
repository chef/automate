-module(deliv_phase_run_integration_tests).

-include_lib("hoax/include/hoax.hrl").
-include("../../src/deliv_stage.hrl").

-compile(export_all).

-define(assertTimestampIsApproximatelyCurrentTime(ColumnName, PhaseRun),
        ((fun () ->
              Timestamp = chef_utils:trunc_timestamp(deliv_phase_run:getval(ColumnName, PhaseRun)),
              TimestampInSeconds = calendar:datetime_to_gregorian_seconds(Timestamp),
              Now = calendar:universal_time(),
              NowInSeconds = calendar:datetime_to_gregorian_seconds(Now),

              case NowInSeconds - TimestampInSeconds < 5 of
                  true -> ok;
                  _ -> erlang:error({assertTimestampIsApproximatelyCurrentTime_failed,
                                     [{module, ?MODULE},
                                      {line, ?LINE},
                                      {timestamp, Timestamp},
                                      {current_time, Now}]})
              end
        end)())).

fixture_test_() ->
    hoax:parameterized_fixture(?MODULE, setup, teardown).

setup_change(Enterprise, Organization, Project, Pipeline, User, FeatureBranch) ->
    ChangePatchset = eu_data:create_patchset(Enterprise, User, Organization,
                                             Project, Pipeline, FeatureBranch),
    Change = eu_data:change_from_patchset(ChangePatchset),
    SubmittedAtChange = deliv_change:getval(submitted_at, Change),
    UpdatedChange = deliv_change:setvals([{submitted_at,
                                           chef_utils:trunc_timestamp(SubmittedAtChange)}], Change),
    UpdatedChange.

setup() ->
    error_logger:tty(false),
    application:start(gproc),
    eu_database:setup(),
    eu_data:with_enterprise(<<"deliv_phase_test_enterprise">>,
      eu_data:with_organization(<<"deliv_phase_test_organization">>,
        eu_data:with_project(<<"deliv_phase_test_project">>,
          eu_data:with_pipeline(<<"master">>,
            fun(Enterprise, Organization, Project, Pipeline) ->
                User = eu_data:fetch_or_create_user(Enterprise, <<"deliv_change_eunit_user">>),
                Change = setup_change(Enterprise, Organization, Project, Pipeline, User, "not_master"),
                [StageRun] = deliv_stage_run:insert([
                            {change_id, deliv_change:getval(id, Change)},
                            {stage, <<"build">>},
                            {status, <<"idle">>}]),
                StageRunId = deliv_stage_run:getval(id, StageRun),
                [PhaseRun] = deliv_phase_run:insert([{stage_run_id, StageRunId}]),

                PhaseRun
            end)))).

teardown(_) ->
    application:stop(gproc),
    eu_database:teardown(),
    error_logger:tty(true),
    ok.

start_updates_status_and_build_node(PhaseRun) ->
    Node = <<"workflow_runner">>,

    [UpdatedPhaseRun] = deliv_phase_run:start(Node, PhaseRun),

    ?assertTimestampIsApproximatelyCurrentTime(started_at, UpdatedPhaseRun),
    ?assertEqual(deliv_phase_run:getval(status, UpdatedPhaseRun), <<"running">>),
    ?assertEqual(deliv_phase_run:getval(build_node, UpdatedPhaseRun), Node).

finish_with_failed_sets_status_and_finished(PhaseRun) ->
    Msg = <<"I am a message">>,
    {ok, UpdatedPhaseRun} = deliv_phase_run:finish(failed, Msg, PhaseRun),

    ?assertTimestampIsApproximatelyCurrentTime(finished_at, UpdatedPhaseRun),
    ?assertEqual(deliv_phase_run:getval(finished, UpdatedPhaseRun), true),
    ?assertEqual(deliv_phase_run:getval(status, UpdatedPhaseRun), <<"failed">>),
    ?assertEqual(deliv_phase_run:getval(run_log, UpdatedPhaseRun), Msg).

finish_with_skipped_sets_status_and_finished(PhaseRun) ->
    Msg = <<"I am a message">>,
    {ok, UpdatedPhaseRun} = deliv_phase_run:finish(skipped, Msg, PhaseRun),

    ?assertTimestampIsApproximatelyCurrentTime(finished_at, UpdatedPhaseRun),
    ?assertEqual(deliv_phase_run:getval(finished, UpdatedPhaseRun), true),
    ?assertEqual(deliv_phase_run:getval(status, UpdatedPhaseRun), <<"skipped">>),
    ?assertEqual(deliv_phase_run:getval(run_log, UpdatedPhaseRun), Msg).

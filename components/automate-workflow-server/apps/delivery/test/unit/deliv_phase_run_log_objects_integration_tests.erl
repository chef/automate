-module(deliv_phase_run_log_objects_integration_tests).

-include_lib("hoax/include/hoax.hrl").

-compile([export_all]).

fetch_parameterized_fixture_test_() ->
    hoax:parameterized_fixture(?MODULE, "fetch_", setup, teardown).

save_parameterized_fixture_test_() ->
    hoax:parameterized_fixture(?MODULE, "save_", setup, teardown).

setup() ->
    error_logger:tty(false),
    application:start(gproc),
    eu_database:setup(),
    eu_data:with_enterprise(<<"Chewbacca">>,
      eu_data:with_organization(<<"WillRoar">>,
        eu_data:with_project(<<"deliv_phase_test_project">>,
          eu_data:with_pipeline(<<"master">>,
            fun(Enterprise, Organization, Project, Pipeline) ->
                User = eu_data:fetch_or_create_user(Enterprise, <<"deliv_phase_eunit_user">>),

                FeatureBranch = <<"deliv_changeset_eunit_feature">>,
                Patchset = eu_data:create_patchset(Enterprise, User, Organization, Project, Pipeline, FeatureBranch),
                Change = eu_data:change_from_patchset(Patchset),
                ChangeId = deliv_change:getval(id, Change),
                [StageRun] = deliv_stage_run:insert([{change_id, ChangeId}, {stage, <<"verify">>}, {status, <<"running">>}]),
                StageRunId = deliv_stage_run:getval(id, StageRun),
                deliv_phase_run:insert([{phase, <<"unit">>}, {stage_run_id, StageRunId}])

            end)))).

teardown(_) ->
    eu_database:teardown(),
    application:stop(gproc),
    error_logger:tty(true).

save_log_object_from_phase_run([PhaseRun]) ->
    RunId = deliv_phase_run:getval(id, PhaseRun),
    RunId2 = erlang:integer_to_binary(RunId),
    Data = <<"{\"compliance\": \"v1.0\",
                \"test_results\": [
                    {
                    \"the_first\": \"passed\"
                    },
                    {
                    \"the_second\": \"did not pass\"
                    }]
            }">>,

    Result = deliv_phase_run_log:save(RunId2, Data),

    ?assertMatch([{deliv_phase_run_log, _, RunId, Data}], Result).

fetch_returns_the_log_object_when_it_exists([PhaseRun]) ->
    RunId = deliv_phase_run:getval(id, PhaseRun),
    RunId2 = erlang:integer_to_binary(RunId),
    Data = <<"{\"compliance\": \"v1.0\",
                \"test_results\": [
                    {
                    \"the_first\": \"passed\"
                    },
                    {
                    \"the_second\": \"did not pass\"
                    }]
            }">>,

    deliv_phase_run_log:save(RunId2, Data),

    ?assertMatch({ok, [{deliv_phase_run_log, _, RunId, Data}]}, deliv_phase_run_log:fetch(RunId2)).

fetch_returns_an_empty_list_when_the_log_object_does_not_exist([PhaseRun]) ->
    RunId = deliv_phase_run:getval(id, PhaseRun),
    RunId2 = erlang:integer_to_binary(RunId),
    Data = <<"{\"compliance\": \"v1.0\",
                \"test_results\": [
                    {
                    \"the_first\": \"passed\"
                    },
                    {
                    \"the_second\": \"did not pass\"
                    }]
            }">>,
    RunId3 = <<"1999978">>,

    deliv_phase_run_log:save(RunId2, Data),
    ?assertMatch({ok, []}, deliv_phase_run_log:fetch(RunId3)).

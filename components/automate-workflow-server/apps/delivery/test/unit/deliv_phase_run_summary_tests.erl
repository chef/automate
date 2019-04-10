-module(deliv_phase_run_summary_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("delivery/include/deliv_types.hrl").

-compile([export_all]).

to_ejson_fixture_test_() ->
    eunit_sugar:fixture(?MODULE, to_ejson_).

to_ejson_when_provided_empty_list_returns_empty_list() ->
    ?assertEqual({ok, []}, deliv_phase_run_summary:to_ejson({ok, []})).

to_ejson_with_urlfun_returns_ejson_with_url() ->
    PhaseRunSummary = #phase_run_summary{
        stage_id = 1, stage = <<"verify">>, stage_status = <<"failed">>,
        phase_id = 3, phase = <<"unit">>, phase_status = <<"passed">>,
        search_query = <<"[builder*]">>,
        search_description = <<"An awesome search">>
    },

    Expected = {ok, [
        {[
            {<<"stage">>, <<"verify">>},
            {<<"status">>, <<"failed">>},
            {<<"phases">>, [
                {[
                    {<<"name">>, <<"unit">>},
                    {<<"status">>, <<"passed">>},
                    {<<"run_details">>, [
                        {[
                            {<<"status">>, <<"passed">>},
                            {<<"description">>, undefined},
                            {<<"href">>, <<"/api/v0/e/fake_ent/orgs/fake_org/projects/fake_proj/pipelines/fake_pipe/phase_runs/3">>},
                            {<<"search_query">>, <<"[builder*]">>},
                            {<<"search_description">>, <<"An awesome search">>}
                        ]}
                    ]}
                ]}
            ]}
        ]}
    ]},

    Result = deliv_phase_run_summary:to_ejson({ok, [PhaseRunSummary]},
        fun(PhaseId) ->
            {EntName, OrgName, ProjName, Pipeline} =
                {<<"fake_ent">>, <<"fake_org">>, <<"fake_proj">>, <<"fake_pipe">>},
            deliv_web_utils:relative_href_for(phase_run, [EntName, OrgName, ProjName,
                Pipeline, PhaseId])
        end),

    ?assertEqual(Expected, Result).

to_ejson_with_urlfun_and_job_urlfun_returns_ejson_with_url() ->
    PhaseRunSummary = #phase_run_summary{
        stage_id = 1, stage = <<"verify">>, stage_status = <<"failed">>,
        phase_id = 3, phase = <<"unit">>, phase_status = <<"passed">>,
        search_query = <<"[builder*]">>,
        search_description = <<"An awesome search">>
    },

    Expected = {ok, [
        {[
            {<<"stage">>, <<"verify">>},
            {<<"status">>, <<"failed">>},
            {<<"phases">>, [
                {[
                    {<<"name">>, <<"unit">>},
                    {<<"status">>, <<"passed">>},
                    {<<"run_details">>, [
                        {[
                            {<<"status">>, <<"passed">>},
                            {<<"description">>, undefined},
                            {<<"href">>, <<"/api/v0/e/fake_ent/orgs/fake_org/projects/fake_proj/pipelines/fake_pipe/phase_runs/3">>},
                            {<<"job_href">>, <<"/api/v0/e/fake_ent/jobs/jobid1">>},
                            {<<"search_query">>, <<"[builder*]">>},
                            {<<"search_description">>, <<"An awesome search">>}
                        ]}
                    ]}
                ]}
            ]}
        ]}
    ]},

    Result = deliv_phase_run_summary:to_ejson({ok, [PhaseRunSummary]},
        fun(PhaseId) ->
            {EntName, OrgName, ProjName, Pipeline} =
                {<<"fake_ent">>, <<"fake_org">>, <<"fake_proj">>, <<"fake_pipe">>},
            deliv_web_utils:relative_href_for(phase_run, [EntName, OrgName, ProjName,
                Pipeline, PhaseId])
        end,
        fun(_StageId, _PhaseId) ->
            <<"/api/v0/e/fake_ent/jobs/jobid1">>
        end),

    ?assertEqual(Expected, Result).

%% convert a list of phase run summaries to ejson and return the phase group
%% status of the phase run summary ejson.
%% assumes generated ejson has one stage with one phase.
phase_group_status(PhaseRunSummary) ->
    {ok, [PhaseRunSummaryEjson]} = deliv_phase_run_summary:to_ejson({ok,
        PhaseRunSummary}),
    [PhaseGroupEjson] = ej:get({<<"phases">>}, PhaseRunSummaryEjson),
    ej:get({<<"status">>}, PhaseGroupEjson).

to_ejson_when_phase_failed_sets_phase_group_status_to_failed() ->
    PhaseRunSummary = [
        #phase_run_summary{stage_id = 1, stage = <<"verify">>,
                           stage_status = <<"failed">>, phase_id = 3,
                           phase = <<"unit">>, phase_status = <<"running">>},
        #phase_run_summary{stage_id = 1, stage = <<"verify">>,
                           stage_status = <<"failed">>, phase_id = 1,
                           phase = <<"unit">>, phase_status = <<"failed">>},
        #phase_run_summary{stage_id = 1, stage = <<"verify">>,
                           stage_status = <<"failed">>, phase_id = 2,
                           phase = <<"unit">>, phase_status = <<"passed">>},
        #phase_run_summary{stage_id = 1, stage = <<"verify">>,
                           stage_status = <<"failed">>, phase_id = 1,
                           phase = <<"unit">>, phase_status = <<"skipped">>},
        #phase_run_summary{stage_id = 1, stage = <<"verify">>,
                           stage_status = <<"failed">>, phase_id = 2,
                           phase = <<"unit">>, phase_status = <<"idle">>}
    ],

    PhaseGroupStatus = phase_group_status(PhaseRunSummary),
    ?assertEqual(<<"failed">>, PhaseGroupStatus).

to_ejson_when_phase_running_sets_phase_group_status_to_running() ->
    PhaseRunSummary = [
        #phase_run_summary{stage_id = 1, stage = <<"verify">>,
                           stage_status = <<"running">>, phase_id = 3,
                           phase = <<"unit">>, phase_status = <<"passed">>},
        #phase_run_summary{stage_id = 1, stage = <<"verify">>,
                           stage_status = <<"running">>, phase_id = 2,
                           phase = <<"unit">>, phase_status = <<"running">>},
        #phase_run_summary{stage_id = 1, stage = <<"verify">>,
                           stage_status = <<"running">>, phase_id = 1,
                           phase = <<"unit">>, phase_status = <<"skipped">>},
        #phase_run_summary{stage_id = 1, stage = <<"verify">>,
                           stage_status = <<"running">>, phase_id = 2,
                           phase = <<"unit">>, phase_status = <<"idle">>}
    ],

    PhaseGroupStatus = phase_group_status(PhaseRunSummary),
    ?assertEqual(<<"running">>, PhaseGroupStatus).

to_ejson_when_phase_idle_sets_phase_group_status_to_idle() ->
    PhaseRunSummary = [
        #phase_run_summary{stage_id = 1, stage = <<"verify">>,
                           stage_status = <<"idle">>, phase_id = 2,
                           phase = <<"unit">>, phase_status = <<"skipped">>},
        #phase_run_summary{stage_id = 1, stage = <<"verify">>,
                           stage_status = <<"idle">>, phase_id = 2,
                           phase = <<"unit">>, phase_status = <<"idle">>},
        #phase_run_summary{stage_id = 1, stage = <<"verify">>,
                           stage_status = <<"idle">>, phase_id = 1,
                           phase = <<"unit">>, phase_status = <<"skipped">>}
    ],

    PhaseGroupStatus = phase_group_status(PhaseRunSummary),
    ?assertEqual(<<"idle">>, PhaseGroupStatus).

to_ejson_when_all_phases_passed_sets_phase_group_status_to_passed() ->
    PhaseRunSummary = [
        #phase_run_summary{stage_id = 1, stage = <<"verify">>,
                           stage_status = <<"passed">>, phase_id = 1,
                           phase = <<"unit">>, phase_status = <<"passed">>},
        #phase_run_summary{stage_id = 1, stage = <<"verify">>,
                           stage_status = <<"passed">>, phase_id = 2,
                           phase = <<"unit">>, phase_status = <<"passed">>}
    ],

    PhaseGroupStatus = phase_group_status(PhaseRunSummary),
    ?assertEqual(<<"passed">>, PhaseGroupStatus).

to_ejson_when_all_phases_skipped_sets_phase_group_status_to_skipped() ->
    PhaseRunSummary = [
        #phase_run_summary{stage_id = 1, stage = <<"verify">>,
                           stage_status = <<"skipped">>, phase_id = 1,
                           phase = <<"unit">>, phase_status = <<"skipped">>},
        #phase_run_summary{stage_id = 1, stage = <<"verify">>,
                           stage_status = <<"skipped">>, phase_id = 2,
                           phase = <<"unit">>, phase_status = <<"skipped">>}
    ],

    PhaseGroupStatus = phase_group_status(PhaseRunSummary),
    ?assertEqual(<<"skipped">>, PhaseGroupStatus).

%% TODO fix indenting
to_ejson_groups_by_stage_and_preserves_order() ->
    PhaseRunSummary = [
        #phase_run_summary{stage_id = 1, stage = <<"verify">>,
                           stage_status = <<"failed">>, phase_id = 3,
                           phase = <<"unit">>, phase_status = <<"passed">>},
        #phase_run_summary{stage_id = 1, stage = <<"verify">>,
                           stage_status = <<"failed">>, phase_id = 1,
                           phase = <<"lint">>, phase_status = <<"failed">>},
        #phase_run_summary{stage_id = 1, stage = <<"verify">>,
                           stage_status = <<"failed">>, phase_id = 2,
                           phase = <<"syntax">>, phase_status = <<"passed">>},
        #phase_run_summary{stage_id = 3, stage = <<"union">>,
                           stage_status = <<"passed">>, phase_id = 10,
                           phase = <<"provision">>, phase_status = <<"passed">>},
        #phase_run_summary{stage_id = 3, stage = <<"union">>,
                           stage_status = <<"passed">>, phase_id = 11,
                           phase = <<"deploy">>, phase_status = <<"passed">>},
        #phase_run_summary{stage_id = 3, stage = <<"union">>,
                           stage_status = <<"passed">>, phase_id = 12,
                           phase = <<"smoke">>, phase_status = <<"passed">>},
        #phase_run_summary{stage_id = 4, stage = <<"rehearsal">>,
                           stage_status = <<"passed">>, phase_id = 14,
                           phase = <<"provision">>, phase_status = <<"passed">>},
        #phase_run_summary{stage_id = 4, stage = <<"rehearsal">>,
                           stage_status = <<"passed">>, phase_id = 15,
                           phase = <<"deploy">>, phase_status = <<"passed">>},
        #phase_run_summary{stage_id = 4, stage = <<"rehearsal">>,
                           stage_status = <<"passed">>, phase_id = 16,
                           phase = <<"smoke">>, phase_status = <<"passed">>}
    ],

    Expected = {ok, [
        {[
            {<<"stage">>, <<"verify">>},
            {<<"status">>, <<"failed">>},
            {<<"phases">>, [
                {[
                    {<<"name">>, <<"unit">>},
                    {<<"status">>, <<"passed">>},
                    {<<"run_details">>, [
                        {[
                            {<<"phase">>, <<"unit">>},
                            {<<"status">>, <<"passed">>}
                        ]}
                    ]}
                ]},
                {[
                    {<<"name">>, <<"lint">>},
                    {<<"status">>, <<"failed">>},
                    {<<"run_details">>, [
                        {[
                            {<<"phase">>, <<"lint">>},
                            {<<"status">>, <<"failed">>}
                        ]}
                    ]}
                ]},
                {[
                    {<<"name">>, <<"syntax">>},
                    {<<"status">>, <<"passed">>},
                    {<<"run_details">>, [
                        {[
                            {<<"phase">>, <<"syntax">>},
                            {<<"status">>, <<"passed">>}
                        ]}
                    ]}
                ]}
            ]}
        ]},
        {[
            {<<"stage">>, <<"union">>},
            {<<"status">>, <<"passed">>},
            {<<"phases">>, [
                {[
                    {<<"name">>, <<"provision">>},
                    {<<"status">>, <<"passed">>},
                    {<<"run_details">>, [
                        {[
                            {<<"phase">>, <<"provision">>},
                            {<<"status">>, <<"passed">>}
                        ]}
                    ]}
                ]},
                {[
                    {<<"name">>, <<"deploy">>},
                    {<<"status">>, <<"passed">>},
                    {<<"run_details">>, [
                        {[
                            {<<"phase">>, <<"deploy">>},
                            {<<"status">>, <<"passed">>}
                        ]}
                    ]}
                ]},
                {[
                    {<<"name">>, <<"smoke">>},
                    {<<"status">>, <<"passed">>},
                    {<<"run_details">>, [
                        {[
                            {<<"phase">>, <<"smoke">>},
                            {<<"status">>, <<"passed">>}
                        ]}
                    ]}
                ]}
            ]}
        ]},
        {[
            {<<"stage">>, <<"rehearsal">>},
            {<<"status">>, <<"passed">>},
            {<<"phases">>, [
                {[
                    {<<"name">>, <<"provision">>},
                    {<<"status">>, <<"passed">>},
                    {<<"run_details">>, [
                        {[
                            {<<"phase">>, <<"provision">>},
                            {<<"status">>, <<"passed">>}
                        ]}
                    ]}
                ]},
                {[
                    {<<"name">>, <<"deploy">>},
                    {<<"status">>, <<"passed">>},
                    {<<"run_details">>, [
                        {[
                            {<<"phase">>, <<"deploy">>},
                            {<<"status">>, <<"passed">>}
                        ]}
                    ]}
                ]},
                {[
                    {<<"name">>, <<"smoke">>},
                    {<<"status">>, <<"passed">>},
                    {<<"run_details">>, [
                        {[
                            {<<"phase">>, <<"smoke">>},
                            {<<"status">>, <<"passed">>}
                        ]}
                    ]}
                ]}
            ]}
        ]}
    ]},

    Result = deliv_phase_run_summary:to_ejson({ok, PhaseRunSummary}),

    ?assertEqual(Expected, Result).

-module(deliv_stage_project_v2_tests).

-include("../../src/deliv_stage.hrl").
-include_lib("hoax/include/hoax.hrl").

-compile(export_all).

deliv_stage_project_v2_test_() ->
    [
     hoax:fixture(?MODULE, "create_phases_from_phase_group_")
    ].

create_phases_from_phase_group_populates_running_phases_with_started_phase_pids() ->
    State = #deliv_stage{change = change,
                         patchset = patchset,
                         stage_run = stage_run,
                         project_config = config,
                         skip_phase_groups = []},
    Criteria = #deliv_ssh_job_criteria{},

    hoax:expect(receive
                    deliv_proj_config:get_phase_filters_from_config(config, <<"unit">>) -> [Criteria];
                    deliv_proj_config:criteria_to_json(Criteria) -> serialized;
                    deliv_proj_config:get_timeout_for_phase(config, <<"unit">>) -> 3600;
                    deliv_phase:start_link(change, patchset, stage_run, self(), <<"unit">>, undefined, serialized, false, ?JOB_DISPATCH_V2, Criteria, 3600) -> {ok, pid}
                end),

    Actual = deliv_stage_project_v2:create_phases_from_phase_group([unit], State),

    ?assertEqual(State#deliv_stage{running_phases=[pid]}, Actual),
    ?verifyAll.

create_phases_from_phase_group_populates_running_phases_with_multiple_phases() ->
    State = #deliv_stage{change = change,
                         patchset = patchset,
                         stage_run = stage_run,
                         project_config = config,
                         skip_phase_groups = []},
    Criteria = #deliv_ssh_job_criteria{},

    hoax:expect(receive
                    deliv_proj_config:get_phase_filters_from_config(config, <<"unit">>) -> [Criteria];
                    deliv_proj_config:get_phase_filters_from_config(config, <<"lint">>) -> [Criteria];
                    deliv_proj_config:get_timeout_for_phase(config, <<"unit">>) -> 3600;
                    deliv_proj_config:get_timeout_for_phase(config, <<"lint">>) -> 3601;
                    deliv_proj_config:criteria_to_json(Criteria) -> serialized;
                    deliv_phase:start_link(change, patchset, stage_run, self(), <<"unit">>, undefined, serialized, false, ?JOB_DISPATCH_V2, Criteria, 3600) -> {ok, pid1};
                    deliv_phase:start_link(change, patchset, stage_run, self(), <<"lint">>, undefined, serialized, false, ?JOB_DISPATCH_V2, Criteria, 3601) -> {ok, pid2}
                end),

    Actual = deliv_stage_project_v2:create_phases_from_phase_group([unit, lint], State),

    ?assertEqual(State#deliv_stage{running_phases=[pid2, pid1]}, Actual),
    ?verifyAll.

create_phases_from_phase_group_populates_running_phases_with_multiple_phases_and_jobs() ->
    State = #deliv_stage{change = change,
                         patchset = patchset,
                         stage_run = stage_run,
                         project_config = config,
                         skip_phase_groups = []},
    Criteria = #deliv_ssh_job_criteria{},

    hoax:expect(receive
                    deliv_proj_config:get_phase_filters_from_config(config, <<"unit">>) -> [Criteria, Criteria];
                    deliv_proj_config:get_phase_filters_from_config(config, <<"lint">>) -> [Criteria];
                    deliv_proj_config:get_phase_filters_from_config(config, <<"syntax">>) -> [Criteria, Criteria];
                    deliv_proj_config:get_timeout_for_phase(config, <<"unit">>) -> 3600;
                    deliv_proj_config:get_timeout_for_phase(config, <<"lint">>) -> 3601;
                    deliv_proj_config:get_timeout_for_phase(config, <<"syntax">>) -> 3602;
                    deliv_proj_config:criteria_to_json(Criteria) -> serialized;
                    deliv_phase:start_link(change, patchset, stage_run, self(), <<"unit">>, undefined, serialized, false, ?JOB_DISPATCH_V2, Criteria, 3600) -> {ok, pid1};
                    deliv_phase:start_link(change, patchset, stage_run, self(), <<"unit">>, undefined, serialized, false, ?JOB_DISPATCH_V2, Criteria, 3600) -> {ok, pid2};
                    deliv_phase:start_link(change, patchset, stage_run, self(), <<"lint">>, undefined, serialized, false, ?JOB_DISPATCH_V2, Criteria, 3601) -> {ok, pid3};
                    deliv_phase:start_link(change, patchset, stage_run, self(), <<"syntax">>, undefined, serialized, false, ?JOB_DISPATCH_V2, Criteria, 3602) -> {ok, pid4};
                    deliv_phase:start_link(change, patchset, stage_run, self(), <<"syntax">>, undefined, serialized, false, ?JOB_DISPATCH_V2, Criteria, 3602) -> {ok, pid5}
                end),

    Actual = deliv_stage_project_v2:create_phases_from_phase_group([unit, lint, syntax], State),

    ?assertEqual(State#deliv_stage{running_phases=[pid4, pid5, pid3, pid1, pid2]}, Actual),
    ?verifyAll.

create_phases_from_phase_group_skips_configured_phase() ->
    State = #deliv_stage{change = change,
                         patchset = patchset,
                         stage_run = stage_run,
                         project_config = config,
                         skip_phase_groups = [<<"unit">>]},
    Criteria = #deliv_ssh_job_criteria{},

    hoax:expect(receive
                    deliv_proj_config:get_phase_filters_from_config(config, <<"unit">>) -> [Criteria];
                    deliv_proj_config:get_timeout_for_phase(config, <<"unit">>) -> 3600;
                    deliv_proj_config:criteria_to_json(Criteria) -> serialized;
                    deliv_phase:start_link(change, patchset, stage_run, self(), <<"unit">>, undefined, serialized, true, ?JOB_DISPATCH_V2, #deliv_ssh_job_criteria{}, 3600) -> {ok, pid}
                end),

    Actual = deliv_stage_project_v2:create_phases_from_phase_group([unit], State),

    ?assertEqual(State#deliv_stage{running_phases=[pid]}, Actual),
    ?verifyAll.

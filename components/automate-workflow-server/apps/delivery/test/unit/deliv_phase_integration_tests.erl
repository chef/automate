-module(deliv_phase_integration_tests).

-include_lib("hoax/include/hoax.hrl").

-include("../../src/deliv_phase.hrl").
-include("../../src/deliverance_types.hrl").

-compile([export_all]).

fixture_test_() ->
    hoax:parameterized_fixture(?MODULE, 'when_', setup, teardown).

setup() ->
    error_logger:tty(false),
    application:start(gproc),
    eu_database:setup(),
    application:set_env(delivery, phase_job_confirmation_timeout, 10),
    eu_data:with_enterprise(<<"deliv_phase_test_enterprise">>,
      eu_data:with_organization(<<"deliv_phase_test_organization">>,
        eu_data:with_project(<<"deliv_phase_test_project">>,
          eu_data:with_pipeline(<<"master">>,
            fun(Enterprise, Organization, Project, Pipeline) ->
                    User = eu_data:fetch_or_create_user(Enterprise, <<"deliv_phase_eunit_user">>),
                    _Builder = eu_data:fetch_or_create_user(Enterprise, <<"builder">>),

                    FeatureBranch = <<"deliv_changeset_eunit_feature">>,
                    Patchset = eu_data:create_patchset(Enterprise, User, Organization, Project, Pipeline, FeatureBranch),
                    Change = eu_data:change_from_patchset(Patchset),

                    ChangeId = deliv_change:getval(id, Change),
                    [StageRun] = deliv_stage_run:insert([{change_id, ChangeId}, {stage, <<"verify">>}, {status, <<"running">>}]),
                    StageRunPid = list_to_pid("<0.1.0>"),
                    Phase = <<"unit">>,
                    SearchQuery = <<"name:search_query">>,
                    SearchDescription = <<"search_description">>,

                    deliv_event:subscribe(phase_state_event),
                    {ok, Pid} = deliv_phase:start_link(Change, Patchset, StageRun, StageRunPid, Phase, SearchQuery, SearchDescription, false, ?JOB_DISPATCH_V1, no_filter, undefined),
                    unlink(Pid),
                    {_, InitialState} = capture_fsm_state(),
                    deliv_event:unsubscribe(phase_state_event),
                    {Pid, InitialState}
            end)))).

teardown({Pid, _}) ->
    erlang:exit(Pid, shutdown),
    application:stop(gproc),
    eu_database:teardown(),
    error_logger:tty(true),
    ok.

%%
%% Test Helpers
%%

force_fsm_into_state(Pid, State, Data) ->
    deliv_event:subscribe(phase_state_event),
    ok = gen_fsm:sync_send_all_state_event(Pid, {force_state, State, Data}).

capture_fsm_state() ->
    receive
        {_, phase_state_event, Result} ->
            Result;
        Other ->
            ?debugFmt("Other Message Received ~p", [Other]),
            {none, #deliv_phase{}}
    after
        500 ->
            erlang:error("Timeout waiting for capture_fsm_state")
    end.

%%
%% Helper Assertions
%%

assertPhaseEventPublished(Msg, Status, Reason, JobStatus, State) ->
    PhaseEvent = #phase_event{},
    hoax:expect(receive
                  deliv_phase_event:new(Status, Reason, JobStatus, State) -> PhaseEvent;
                  deliv_phase_event:publish(Msg, PhaseEvent) -> ignored
                end).

assertCompletePhase(Pid, #deliv_phase{stage_run_pid = StageRunPid} = State, Status) ->
    hoax:mock(deliv_stage,
              ?expect(phase_finished,
                      ?withArgs([StageRunPid, Pid, Status]))),

    hoax:mock(deliv_phase_job,
              ?expect(stop,
                      ?withArgs([State]),
                      ?andReturn(ok))).


%%
%% Tests
%%

when_in_skip_state_start_action_terminates_fsm({Pid, #deliv_phase{stage_run_pid = StageRunPid,
                                                                  phase_run = PhaseRun} = InitialState}) ->
    force_fsm_into_state(Pid, skip, InitialState),

    ExpectedPhaseRun = deliv_phase_run:setvals([
        {finished, true},
        {status, <<"skipped">>},
        {run_log, <<"Skipped phase">>},
        {finished_at, {{2017,4,18},{12,22,23.0}}}
    ], PhaseRun),

    ExpectedState = InitialState#deliv_phase{phase_run = ExpectedPhaseRun},

    assertPhaseEventPublished(finished, skipped, undefined, undefined, ExpectedState),

    hoax:expect(receive
                  deliv_stage:phase_finished(StageRunPid, Pid, skipped) -> ignored;
                  calendar:universal_time() -> {{2017,4,18},{12,22,23}}
                end),

    ok = deliv_phase:run(Pid),
    {NextState, ActualState} = capture_fsm_state(),

    ?assertEqual(none, NextState),
    ?assertEqual(ExpectedState, ActualState),
    ?verifyAll.

when_in_idle_state_start_action_transitions_fsm_to_waiting_worker({Pid, InitialState}) ->
    force_fsm_into_state(Pid, idle, InitialState),

    PhaseJobPid = list_to_pid("<0.2.0>"),
    hoax:mock(deliv_phase_job,
              ?expect(dispatch,
                      ?withArgs([InitialState]),
                      ?andReturn({ok, PhaseJobPid}))),

    ExpectedState = InitialState#deliv_phase{phase_job_pid = PhaseJobPid},

    assertPhaseEventPublished(waiting_for_worker, idle, waiting_for_worker, undefined, ExpectedState),

    ok = deliv_phase:run(Pid),

    {NextState, ActualState} = capture_fsm_state(),

    ?assertEqual(waiting_worker, NextState),
    ?assertEqual(ExpectedState, ActualState),
    ?verifyAll.

when_in_waiting_worker_update_action_with_no_worker_continues_waiting_worker({Pid, InitialState}) ->
    force_fsm_into_state(Pid, waiting_worker, InitialState),

    PushJobStatus = #push_job_status{status = <<"voting">>},
    PhaseJobStatus = #phase_job_status{started = false, status = <<"voting">>},

    hoax:mock(deliv_phase_job,
              ?expect(status,
                      ?withArgs([PushJobStatus]),
                      ?andReturn(PhaseJobStatus))),

    ok = deliv_phase:update(Pid, PushJobStatus),
    {NextState, ActualState} = capture_fsm_state(),

    ?assertEqual(waiting_worker, NextState),
    ?assertEqual(InitialState, ActualState),
    ?verifyAll.

when_in_waiting_worker_update_action_with_worker_update_transitions_to_running({Pid, #deliv_phase{phase_run = PhaseRun} = InitialState}) ->
    force_fsm_into_state(Pid, waiting_worker, InitialState),

    Node = <<"build_node_name">>,
    JobStatus = <<"running">>,
    PushJobStatus = #push_job_status{status = JobStatus},
    PhaseJobStatus = #phase_job_status{status = JobStatus, started = true, node = Node},

    ExpectedPhaseRun = deliv_phase_run:setvals([
        {build_node, Node},
        {status, <<"running">>},
        {started_at, {{2017,4,18},{12,22,23.0}}}
    ], PhaseRun),
    ExpectedState = InitialState#deliv_phase{phase_run = ExpectedPhaseRun},

    hoax:expect(receive
                  deliv_phase_job:status(PushJobStatus) -> PhaseJobStatus;
                  calendar:universal_time() -> {{2017,4,18},{12,22,23}}
                end),

    assertPhaseEventPublished(started, running, undefined, JobStatus, ExpectedState),

    ok = deliv_phase:update(Pid, PushJobStatus),
    {NextState, ActualState} = capture_fsm_state(),

    ?assertEqual(running, NextState),
    ?assertEqual(ExpectedState, ActualState),
    ?verifyAll.

when_in_state_other_than_waiting_log_phase_updated_event_has_fsm_keep_that_state({Pid, #deliv_phase{phase_run = PhaseRun} = InitialState}) ->
    force_fsm_into_state(Pid, waiting_worker, InitialState),

    PhaseId = deliv_phase_run:getval(id, PhaseRun),
    deliv_event:publish({phase_run, PhaseId}, updated),

    {NextState, ActualState} = capture_fsm_state(),

    ?assertEqual(waiting_worker, NextState),
    ?assertEqual(InitialState, ActualState).

when_in_waiting_worker_no_worker_action_terminates_fsm({Pid, #deliv_phase{phase_run = PhaseRun} = State}) ->
    StageRunPid = list_to_pid("<0.2.0>"),
    InitialState = State#deliv_phase{stage_run_pid = StageRunPid},
    force_fsm_into_state(Pid, waiting_worker, InitialState),

    Status = failed,
    ExpectedPhaseRun = deliv_phase_run:setvals([
        {finished, true},
        {status, chef_utils:to_bin(Status)},
        {run_log, <<"Could not start phase run. No build nodes available">>},
        {finished_at, {{2017,4,18},{12,22,23.0}}}
    ], PhaseRun),
    ExpectedState = InitialState#deliv_phase{phase_run = ExpectedPhaseRun},

    hoax:expect(receive
                  calendar:universal_time() -> {{2017,4,18},{12,22,23}}
                end),

    assertPhaseEventPublished(finished, failed, no_workers, undefined, ExpectedState),
    assertCompletePhase(Pid, ExpectedState, Status),

    ok = deliv_phase:no_worker(Pid),
    {NextState, ActualState} = capture_fsm_state(),

    ?assertEqual(none, NextState),
    ?assertEqual(ExpectedState, ActualState),
    ?verifyAll.

when_in_running_update_action_with_new_job_status_fires_event_and_continues_running({Pid, State}) ->
    InitialState = State#deliv_phase{job_status = <<"idle">>},
    force_fsm_into_state(Pid, running, InitialState),

    JobStatus = <<"running">>,
    PushJobStatus = #push_job_status{status = JobStatus},
    PhaseJobStatus = #phase_job_status{status = JobStatus, started = true},
    ExpectedState = InitialState#deliv_phase{job_status = JobStatus},

    hoax:mock(deliv_phase_job,
              ?expect(status,
                      ?withArgs([PushJobStatus]),
                      ?andReturn(PhaseJobStatus))),

    assertPhaseEventPublished(updated, running, undefined, JobStatus, ExpectedState),

    ok = deliv_phase:update(Pid, PushJobStatus),
    {NextState, ActualState} = capture_fsm_state(),

    ?assertEqual(running, NextState),
    ?assertEqual(ExpectedState, ActualState),
    ?verifyAll.

when_in_running_update_action_with_same_job_status_does_not_fire_event_and_continues_running({Pid, State}) ->
    InitialState = State#deliv_phase{job_status = <<"running">>},
    force_fsm_into_state(Pid, running, InitialState),

    JobStatus = <<"running">>,
    PushJobStatus = #push_job_status{status = JobStatus},
    PhaseJobStatus = #phase_job_status{status = JobStatus, started = true},
    ExpectedState = InitialState#deliv_phase{job_status = JobStatus},

    hoax:mock(deliv_phase_job,
              ?expect(status,
                      ?withArgs([PushJobStatus]),
                      ?andReturn(PhaseJobStatus))),

    hoax:mock(deliv_phase_event,
              ?expect(publish,
                      ?withArgs([?any, ?any]),
                      ?times(0))),

    ok = deliv_phase:update(Pid, PushJobStatus),
    {NextState, ActualState} = capture_fsm_state(),

    ?assertEqual(running, NextState),
    ?assertEqual(ExpectedState, ActualState),
    ?verifyAll.

when_in_running_job_finished_action_transitions_to_waiting_log_then_times_out({Pid, State}) ->
    PhaseJobPid = list_to_pid("<0.2.0>"),
    InitialState = State#deliv_phase{phase_job_pid = PhaseJobPid},
    force_fsm_into_state(Pid, running, InitialState),

    Status = failed,
    assertPhaseEventPublished(finished, Status, undefined, undefined, InitialState),
    assertCompletePhase(Pid, InitialState, Status),

    ok = deliv_phase:finished(Pid),

    %% capture transition to waiting_log
    {PreTimeoutNextState, PreTimeoutActualState} = capture_fsm_state(),

    %% capture transition to timeout (1 second)
    {TimeoutNextState, TimeoutActualState} = capture_fsm_state(),

    ?assertEqual(waiting_log, PreTimeoutNextState),
    ?assertEqual(InitialState, PreTimeoutActualState),
    ?assertEqual(none, TimeoutNextState),
    ?assertEqual(InitialState, TimeoutActualState),
    ?verifyAll.

when_in_running_phase_run_event_with_pass_fail_status_finishes_phase({Pid, #deliv_phase{phase_run = PhaseRun} = State}) ->
    PhaseJobPid = list_to_pid("<0.2.0>"),
    InitialState = State#deliv_phase{phase_job_pid = PhaseJobPid},
    force_fsm_into_state(Pid, running, InitialState),
    Status = passed,

    assertPhaseEventPublished(finished, Status, undefined, undefined, InitialState),
    assertCompletePhase(Pid, InitialState, Status),

    PhaseId = deliv_phase_run:getval(id, PhaseRun),
    deliv_event:publish({phase_run, PhaseId}, Status),

    {NextState, ActualState} = capture_fsm_state(),

    ?assertEqual(none, NextState),
    ?assertEqual(InitialState, ActualState),
    ?verifyAll.

when_in_running_phase_run_event_with_update_keeps_running_without_timeout({Pid, #deliv_phase{phase_run = PhaseRun} = State}) ->
    PhaseJobPid = list_to_pid("<0.2.0>"),
    InitialState = State#deliv_phase{phase_job_pid = PhaseJobPid},
    force_fsm_into_state(Pid, running, InitialState),

    PhaseId = deliv_phase_run:getval(id, PhaseRun),
    deliv_event:publish({phase_run, PhaseId}, updated),

    {NextState, ActualState} = capture_fsm_state(),

    ?assertEqual(running, NextState),
    ?assertEqual(InitialState, ActualState),
    ?verifyAll.

when_in_waiting_log_phase_run_event_with_update_keeps_waiting_log_then_times_out({Pid, #deliv_phase{phase_run = PhaseRun} = State}) ->
    PhaseJobPid = list_to_pid("<0.2.0>"),
    InitialState = State#deliv_phase{phase_job_pid = PhaseJobPid},
    force_fsm_into_state(Pid, waiting_log, InitialState),

    Status = failed,
    assertPhaseEventPublished(finished, Status, undefined, undefined, InitialState),
    assertCompletePhase(Pid, InitialState, Status),

    PhaseId = deliv_phase_run:getval(id, PhaseRun),
    deliv_event:publish({phase_run, PhaseId}, updated),

    %% capture transition to waiting_log
    {PreTimeoutNextState, PreTimeoutActualState} = capture_fsm_state(),

    %% capture transition to timeout (after 1 second)
    {TimeoutNextState, TimeoutActualState} = capture_fsm_state(),

    ?assertEqual(waiting_log, PreTimeoutNextState),
    ?assertEqual(InitialState, PreTimeoutActualState),
    ?assertEqual(none, TimeoutNextState),
    ?assertEqual(InitialState, TimeoutActualState),
    ?verifyAll.

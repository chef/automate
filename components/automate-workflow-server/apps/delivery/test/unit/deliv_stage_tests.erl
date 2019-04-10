-module(deliv_stage_tests).

-include_lib("hoax/include/hoax.hrl").
-include("../../src/deliv_stage.hrl").

-compile(export_all).

%% Helper functions.

%% Fetch the pipeline id from a change id.
%% (This may be nice to put into deliv_change.)
fetch_pipe_id_by_change_id(ChangeId) ->
    {ok, Change} = deliv_change:fetch_by_id(ChangeId),
    deliv_change:getval(pipeline_id, Change).

subscribe_stage_events_fixture_test_() ->
    hoax:fixture(?MODULE, subscribe_stage_events).

subscribe_stage_events_should_scribe_for_stage_data() ->
    hoax:expect(receive
                    deliv_event:subscribe([
                               {{stage, started}, stage1},
                               {{stage, finished}, stage1},
                               {{stage, running}, stage1},
                               {{stage, started}, stage2},
                               {{stage, finished}, stage2},
                               {{stage, running}, stage2}
                               ]) -> ignored;
                    deliv_stage_data:stages() -> [
                                                  {stage1, do_not_care},
                                                  {stage2, do_not_care}
                                                 ]
                end),

    deliv_stage:subscribe_stage_events(),

    ?verifyAll.

trigger_fixture_test_() ->
    hoax:fixture(?MODULE, trigger).

trigger_verify_stage_when_there_are_no_patchsets() ->
    ChangeId = <<"62794f41-7a0a-40fc-ae15-13b4aae99e8d">>,
    Change = deliv_change:fromlist([{id, ChangeId}]),
    hoax:expect(receive
                    deliv_patchset:latest_patchset_for_change(ChangeId) -> {error, patchset_not_found}
                end),

    ?assertEqual({error, patchset_not_found}, deliv_stage:trigger(verify, Change)),
    ?verifyAll.

stage_with_no_consumers_test_() ->
    hoax:parameterized_fixture(?MODULE, "stage_with_no_consumers", setup_with_no_consumers, teardown).

stage_with_two_consumers_test_() ->
    hoax:parameterized_fixture(?MODULE, "stage_with_two_consumers", setup_with_two_consumers, teardown).

parent_stage_test_() ->
    hoax:parameterized_fixture(?MODULE, "parent_stage", setup_with_two_consumers_and_pids, teardown).

child_stage_test_() ->
    hoax:parameterized_fixture(?MODULE, "child_stage", setup_with_no_consumers, teardown).

setup(ChangeCreationFun) ->
    error_logger:tty(false),
    eu_database:setup(),
    application:start(gproc),
    eu_data:with_enterprise(<<"deliv_stage_test_enterprise">>,
      eu_data:with_organization(<<"deliv_stage_test_organization">>,
        eu_data:with_project(<<"deliv_stage_test_project">>,
          eu_data:with_pipeline(<<"master">>,ChangeCreationFun)))).

teardown(_) ->
    eu_database:teardown(),
    application:stop(gproc),
    error_logger:tty(true).

state_with(Change, Patchset, ConsumerChangeIds) ->
    ChangeId = deliv_change:getval(id, Change),
    ScopingNames = deliv_change:scoping_names(ChangeId),
    Scope = deliv_scopes:from_change_id(ChangeId),

    [StageRun] = deliv_stage_run:insert([{change_id, ChangeId},
                                         {stage, <<"union">>},
                                         {status, <<"idle">>}]),

    ProjConfig = deliv_proj_config_tests:make_full_valid_json(2),

    #deliv_stage{change=Change,
                 patchset=Patchset,
                 stage_run=StageRun,
                 stage_name=union,
                 next_phase_groups=[provision, deploy, smoke, functional],
                 skip_phase_groups=[<<"lint">>, <<"syntax">>],
                 running_phases=[],
                 project_config=ProjConfig,
                 scoping_names=ScopingNames,
                 scope=Scope,
                 consumer_change_ids=ConsumerChangeIds
                }.

setup_with_no_consumers() ->
    setup(fun(Enterprise, Organization, Project, Pipeline) ->
        User = eu_data:fetch_or_create_user(Enterprise, <<"deliv_stage_eunit_user">>),
        FeatureBranch = <<"deliv_stage_eunit_branch1">>,
        Patchset = eu_data:create_patchset(Enterprise, User, Organization, Project, Pipeline, FeatureBranch),
        Change = eu_data:change_with_closed_changeset_from_patchset(Patchset, User, []),
        state_with(Change, Patchset, [])
    end).

setup_with_two_consumers() ->
    setup(fun(Enterprise, Organization, Project, Pipeline) ->
        User = eu_data:fetch_or_create_user(Enterprise, <<"deliv_stage_eunit_user">>),

        FeatureBranch = <<"deliv_stage_eunit_branch2">>,
        Patchset = eu_data:create_patchset(Enterprise, User, Organization, Project, Pipeline, FeatureBranch),
        Change = eu_data:change_with_closed_changeset_from_patchset(Patchset, User, []),

        ConsumerChange1 = eu_data:change_on_consumer_of_pipeline(Enterprise, Organization, Pipeline, User,
        <<"deliv_stage_consumer1">>),
        ConsumerChange2 = eu_data:change_on_consumer_of_pipeline(Enterprise, Organization, Pipeline, User,
        <<"deliv_stage_consumer2">>),

        ConsumerChangeId1 = deliv_change:getval(id, ConsumerChange1),
        ConsumerChangeId2 = deliv_change:getval(id, ConsumerChange2),

        state_with(Change, Patchset, [ConsumerChangeId1, ConsumerChangeId2])
    end).

setup_with_two_consumers_and_pids() ->
    State = setup_with_two_consumers(),
    State#deliv_stage{child_stage_pids=[list_to_pid("<0.1.0>"), list_to_pid("<0.2.0>")]}.

assert_stage_fails_properly(#deliv_stage{stage_run = StageRun,
                                         scoping_names = [EntName, OrgName, ProjName, _],
                                         change = Change} = State,
                            ConsumerPipelineIds) ->

    {ok, NewStageRun} = deliv_stage_run:update([{finished, true}], StageRun),
    FailedState = State#deliv_stage{running_phases=[],
                                    failed=failed},
    ExpectedState = FailedState#deliv_stage{stage_run=NewStageRun},
    ChangeId = deliv_change:getval(id, Change),
    PipeId = deliv_change:getval(pipeline_id, Change),

    UnknownMap = maps:from_list([{P, unknown} || P <- ConsumerPipelineIds]),
    FinalMap = UnknownMap#{ PipeId => failed },

    hoax:expect(receive
        deliv_event:publish({stage_run, failures}, ?any) -> ignored;
        deliv_event:publish({{stage, finished}, union}, ?any) -> ignored;
        deliv_event:publish({build_event_for_change, ChangeId}, Change) -> ignored;
        deliv_event:publish(build_event_for_change, Change) -> ignored;
        deliv_event:publish(union_finished, FinalMap) -> ignored;
        %% Stub out web-url for failure notification
        deliv_web_utils:make_web_url_for_change(EntName, OrgName, ProjName, ChangeId) -> "http://change-url";
        deliv_stage_run:getval(stage, StageRun) -> <<"union">>;
        deliv_stage_run:getval(change_id, StageRun) -> ChangeId;
        deliv_stage_run:finish(StageRun, failed) -> {ok, NewStageRun}
                end),

    Result = deliv_stage:idle(start, FailedState),
    ?assertEqual({stop, normal, ExpectedState}, Result),
    ?verifyAll,
    FailedState.

stage_with_no_consumers_ends_successfully_if_there_are_no_running_phases_and_has_not_failed_yet(
        #deliv_stage{stage_run = StageRun,
                     change = Change} = RawState) ->

    ChangeId = deliv_change:getval(id, Change),
    PipeId = deliv_change:getval(pipeline_id, Change),
    State = RawState#deliv_stage{passed_pipelines = [PipeId]},

    {ok, NewStageRun} =
        deliv_stage_run:update([{finished, true},{status, <<"passed">>}], StageRun),
    IncomingState = State#deliv_stage{running_phases=[],
                                      next_phase_groups=[],
                                      failed=not_failed_yet},
    ExpectedState = IncomingState#deliv_stage{stage_run=NewStageRun},

    hoax:expect(receive
                    deliv_stage_data:stages() -> [{union, {[], none}}];
                    deliv_event:publish({{stage, finished}, union}, ?any) -> ignored;
                    deliv_event:publish({build_event_for_change, ChangeId}, Change) -> ignored;
                    deliv_event:publish(build_event_for_change, Change) -> ignored;
                    deliv_event:publish(union_finished, #{PipeId => passed}) -> ignored;
                    deliv_stage_run:getval(stage, StageRun) -> <<"union">>;
                    deliv_stage_run:getval(change_id, StageRun) -> ChangeId;
                    deliv_stage_run:finish(StageRun, passed) -> {ok, NewStageRun}
                end),

    Result = deliv_stage:idle(start, IncomingState),
    ?assertEqual({stop, normal, ExpectedState}, Result),
    ?verifyAll.

stage_with_no_consumers_ends_unsuccessfully_if_there_are_no_running_phases_and_has_failed(
        #deliv_stage{change = Change} = RawState) ->

    PipeId = deliv_change:getval(pipeline_id, Change),
    State = RawState#deliv_stage{failed_pipelines = [PipeId]},
    assert_stage_fails_properly(State, []).

stage_with_two_consumers_grabs_ids_for_latest_consumer_changes_in_union(
        #deliv_stage{change = Change,
                     patchset = Patchset,
                     stage_run = StageRun,
                     scope = Scope,
                     project_config = ProjConfig} = State) ->

    %% Stub out initial_state to return some pre-determined values
    hoax:mock(deliv_scm_local,
              ?expect(load_config_for_patchset,
                      ?withArgs([Patchset, Scope]),
                      ?andReturn({ok, ProjConfig}))),

    hoax:mock(deliv_stage_run,
              ?expect(insert,
                      ?withArgs([[{change_id, deliv_change:getval(id, Change)},
                                  {stage, <<"union">>},
                                  {status, <<"idle">>}]]),
                      ?andReturn([StageRun]))),

    %% Assert that we will receive the expected initial state
    hoax:mock(deliverance_stage_queue,
              ?expect(in,
                      ?withArgs([State]))),

    deliv_stage:trigger(union, Change),
    ?verifyAll.

stage_with_two_consumers_and_failing_phases_returns_failure_without_running_consumer_tests(
        #deliv_stage{change = Change,
                     consumer_change_ids=[ConsumerChangeId1, ConsumerChangeId2]} = RawState) ->

    PipeId = deliv_change:getval(pipeline_id, Change),
    Consumer1PipeId = fetch_pipe_id_by_change_id(ConsumerChangeId1),
    Consumer2PipeId = fetch_pipe_id_by_change_id(ConsumerChangeId2),
    State = RawState#deliv_stage{failed_pipelines = [PipeId]},
    assert_stage_fails_properly(State, [Consumer2PipeId, Consumer1PipeId]).

stage_with_two_consumers_and_passing_phases_runs_union_substage_for_each_consumer_change_id(
        #deliv_stage{stage_run = StageRun,
                     consumer_change_ids = [ChangeId2, ChangeId3],
                     skip_phase_groups = SkippedPhaseGroups,
                     project_config = ProjConfig,
                     change = Change} = RawState) ->

    ChangeId = deliv_change:getval(id, Change),
    PipeId = deliv_change:getval(pipeline_id, Change),
    State = RawState#deliv_stage{passed_pipelines = [PipeId]},
    {ok, NewStageRun} = deliv_stage_run:update([{status, <<"running">>}], StageRun),

    IncomingState = State#deliv_stage{running_phases = [],
                                      next_phase_groups = [],
                                      stage_run = NewStageRun,
                                      failed = not_failed_yet},

    {ok, Change2} = deliv_change:fetch_by_id(ChangeId2),
    {ok, Change3} = deliv_change:fetch_by_id(ChangeId3),

    {ok, Patchset2} = deliv_patchset:latest_patchset_for_change(ChangeId2),
    {ok, Patchset3} = deliv_patchset:latest_patchset_for_change(ChangeId3),

    Scope2 = deliv_scopes:from_change_id(ChangeId2),
    Scope3 = deliv_scopes:from_change_id(ChangeId3),

    ParentPid = self(),
    Pid1 = list_to_pid("<0.1.0>"),
    Pid2 = list_to_pid("<0.2.0>"),
    ConsumerStage1 = #deliv_stage{change = Change2,
                                  parent_stage_pid = ParentPid,
                                  child_stage_pids = undefined,
                                  patchset = Patchset2,
                                  stage_run = NewStageRun,
                                  stage_name = union,
                                  next_phase_groups = [provision, deploy, smoke, functional],
                                  skip_phase_groups = SkippedPhaseGroups,
                                  running_phases = [],
                                  project_config = ProjConfig,
                                  failed = not_failed_yet,
                                  scoping_names = deliv_change:scoping_names(ChangeId2),
                                  scope = Scope2,
                                  consumer_change_ids = []
                                 },

    ConsumerStage2 = #deliv_stage{change = Change3,
                                  parent_stage_pid = ParentPid,
                                  child_stage_pids = undefined,
                                  patchset = Patchset3,
                                  stage_run = NewStageRun,
                                  stage_name = union,
                                  next_phase_groups = [provision, deploy, smoke, functional],
                                  skip_phase_groups = SkippedPhaseGroups,
                                  running_phases = [],
                                  project_config = ProjConfig,
                                  failed = not_failed_yet,
                                  scoping_names = deliv_change:scoping_names(ChangeId3),
                                  scope = Scope3,
                                  consumer_change_ids = []
                                 },
    hoax:mock(deliv_scm_local,
        [
              ?expect(load_config_for_patchset,
                      ?withArgs([Patchset2, Scope2]),
                      ?andReturn({ok, ProjConfig})),
              ?expect(load_config_for_patchset,
                      ?withArgs([Patchset3, Scope3]),
                      ?andReturn({ok, ProjConfig}))
        ]),

    hoax:mock(gen_fsm,
        [
              ?expect(send_event,
                      ?withArgs([Pid1, start]),
                      ?andReturn(ok)),
              ?expect(send_event,
                      ?withArgs([Pid2, start]),
                      ?andReturn(ok))
        ]),

    hoax:mock(deliv_stage_sup,
        [
              ?expect(start_stage,
                      ?withArgs([ConsumerStage1]),
                      ?andReturn({ok, Pid1})),
              ?expect(start_stage,
                      ?withArgs([ConsumerStage2]),
                      ?andReturn({ok, Pid2}))
        ]),

    hoax:mock(deliv_event,
        [
              ?expect(publish,
                      ?withArgs([{build_event_for_change, ChangeId}, Change])),
              ?expect(publish,
                      ?withArgs([build_event_for_change, Change]))
        ]),

    ExpectedState = IncomingState#deliv_stage{child_stage_pids=[Pid2, Pid1],
                                              stage_run=NewStageRun},
    Result = deliv_stage:idle(start, IncomingState),
    ?assertEqual({next_state, running, ExpectedState}, Result),
    ?verifyAll.

child_stage_notifies_parent_stage_when_phase_fails(#deliv_stage{stage_run = StageRun,
                                                                change = Change} = RawState) ->
    PipeId = deliv_change:getval(pipeline_id, Change),
    ParentPid = list_to_pid("<0.1.0>"),
    PhasePid = list_to_pid("<0.2.0>"),
    ChildState = RawState#deliv_stage{parent_stage_pid = ParentPid,
                                      next_phase_groups = [functional],
                                      running_phases = [PhasePid]},

    {ok, FailedStageRun} = deliv_stage_run:update([{status, <<"failed">>}], StageRun),
    ExpectedState = ChildState#deliv_stage{running_phases = [],
                                           stage_run = FailedStageRun,
                                           failed = failed},

    hoax:mock(gen_fsm,
              ?expect(send_event,
                      ?withArgs([ParentPid, {child_stage_finished, self(), {PipeId, failed}}]))),

    Result = deliv_stage:running({phase_finished, PhasePid, failed}, ChildState),
    ?assertEqual({stop, normal, ExpectedState}, Result),
    ?verifyAll.

child_stage_notifies_parent_stage_when_it_completes(#deliv_stage{change = Change} = RawState) ->
    PipeId = deliv_change:getval(pipeline_id, Change),
    ParentPid = list_to_pid("<0.1.0>"),
    PhasePid = list_to_pid("<0.2.0>"),
    ChildState = RawState#deliv_stage{parent_stage_pid = ParentPid,
                                      next_phase_groups = [],
                                      running_phases = [PhasePid]},

    ExpectedState = ChildState#deliv_stage{running_phases = []},

    hoax:mock(gen_fsm,
              ?expect(send_event,
                      ?withArgs([ParentPid, {child_stage_finished, self(), {PipeId, not_failed_yet}}]))),

    Result = deliv_stage:running({phase_finished, PhasePid, passed}, ChildState),
    ?assertEqual({stop, normal, ExpectedState}, Result),
    ?verifyAll.

child_stage_finishes_all_phase_groups_before_ending(
        #deliv_stage{stage_run = StageRun,
                     change = Change,
                     patchset = Patchset,
                     project_config = ProjectConfig} = RawState) ->
    ParentPipeId = 123,
    PipeId = deliv_change:getval(pipeline_id, Change),
    ParentPid = list_to_pid("<0.1.0>"),
    PhasePid = list_to_pid("<0.2.0>"),
    FunctionalPhasePid = list_to_pid("<0.3.0>"),

    ChildState = RawState#deliv_stage{parent_stage_pid = ParentPid,
                                      next_phase_groups = [functional],
                                      running_phases = [PhasePid],
                                      passed_pipelines = [ParentPipeId]},

    {ok, RunningStageRun} = deliv_stage_run:update([{status, <<"running">>}], StageRun),
    FunctionalPhaseState = ChildState#deliv_stage{next_phase_groups = [],
                                                  stage_run = RunningStageRun,
                                                  running_phases = [FunctionalPhasePid]},

    ExpectedState = FunctionalPhaseState#deliv_stage{next_phase_groups = [],
                                                     running_phases = []},

    ChangeId = deliv_change:getval(id, Change),
    hoax:expect(receive
                    deliv_stage_run:start(StageRun) -> {ok, RunningStageRun};
                    deliv_stage_run:getval(stage, StageRun) -> <<"union">>;
                    deliv_stage_run:getval(change_id, StageRun) -> ChangeId;

                    deliv_event:publish({{stage, running}, union}, ?any) -> ignored;
                    deliv_event:publish({build_event_for_change, ChangeId}, Change) -> ignored;
                    deliv_event:publish(build_event_for_change, Change) -> ignored;
                    deliv_proj_config:job_dispatch_version(ProjectConfig) -> ?JOB_DISPATCH_V1;
                    deliv_proj_config:get_searches_for_phase(<<"functional">>, ProjectConfig) -> [];
                    deliv_proj_config:get_timeout_for_phase(ProjectConfig, <<"functional">>) -> 3600;
                    deliv_phase:start_link(Change, Patchset, StageRun, ?any, <<"functional">>, ?any, ?any, false, ?JOB_DISPATCH_V1, undefined, 3600) -> {ok, FunctionalPhasePid};
                    deliv_phase:run(FunctionalPhasePid) -> ignored;
                    gen_fsm:send_event(ParentPid, {child_stage_finished, self(), {PipeId, not_failed_yet}}) -> ignored;
                    delivery_app:get_env(deliv_default_search) -> <<"search">>
                end),

    Result1 = deliv_stage:running({phase_finished, PhasePid, passed}, ChildState),
    ?assertEqual({next_state, running, FunctionalPhaseState}, Result1),

    Result2 = deliv_stage:running({phase_finished, FunctionalPhasePid, passed}, FunctionalPhaseState),
    ?assertEqual({stop, normal, ExpectedState}, Result2),
    ?verifyAll.

parent_stage_does_not_end_until_all_child_stages_have_ended(
        #deliv_stage{child_stage_pids=[Child1Pid, Child2Pid],
                     consumer_change_ids=[ConsumerChangeId1, ConsumerChangeId2],
                     stage_run=StageRun,
                     change = Change} = RawState) ->

    ChangeId = deliv_change:getval(id, Change),
    PipeId = deliv_change:getval(pipeline_id, Change),
    State = RawState#deliv_stage{passed_pipelines = [PipeId]},

    {ok, NewStageRun} = deliv_stage_run:update([{status, <<"failed">>}], StageRun),

    Consumer1PipeId = fetch_pipe_id_by_change_id(ConsumerChangeId1),
    Consumer2PipeId = fetch_pipe_id_by_change_id(ConsumerChangeId2),

    %% We expect pipeline 1 to fail.
    StateAfterFirstChildFinished = State#deliv_stage{child_stage_pids = [Child2Pid],
                                                     stage_run = NewStageRun,
                                                     failed = failed,
                                                     failed_pipelines = [Consumer1PipeId]},

    {ok, UpdatedNewStageRun} = deliv_stage_run:update([{finished, true}], NewStageRun),
    ExpectedState2 = StateAfterFirstChildFinished#deliv_stage{child_stage_pids = [],
                                                              passed_pipelines = [Consumer2PipeId, PipeId],
                                                              stage_run = UpdatedNewStageRun},

    hoax:expect(receive
        deliv_event:publish({stage_run, failures}, ?any) -> ignored;
        deliv_event:publish({{stage, running}, union}, ?any) -> ignored;
        deliv_event:publish({{stage, finished}, union}, ?any) -> ignored;
        deliv_event:publish({build_event_for_change, ChangeId}, Change) -> ignored;
        deliv_event:publish(build_event_for_change, Change) -> ignored;
        deliv_event:publish(union_finished, #{PipeId => passed, Consumer1PipeId => failed, Consumer2PipeId => passed}) -> ignored;
        deliv_stage_run:update([{status, <<"failed">>}], StageRun) -> {ok, NewStageRun};
        deliv_stage_run:getval(stage, NewStageRun) -> <<"union">>;
        deliv_stage_run:getval(change_id, NewStageRun) -> ChangeId;
        deliv_stage_run:finish(NewStageRun, failed) -> {ok, UpdatedNewStageRun};
        delivery_app:get_env(api_proto) -> <<"https">>;
        delivery_app:get_env(hostname) -> <<"delivery.local">>
                end),

    %% send stage_finished to a parent with multiple children
    Result1 = deliv_stage:running({child_stage_finished, Child1Pid, {Consumer1PipeId, failed}}, State),
    ?assertEqual({next_state, running, StateAfterFirstChildFinished}, Result1),

    %% send another stage_finished for the second child
    Result2 = deliv_stage:running({child_stage_finished, Child2Pid, {Consumer2PipeId, not_failed_yet}}, StateAfterFirstChildFinished),
    ?assertEqual({stop, normal, ExpectedState2}, Result2).

%% The parent stage should continue to the next stage once all its child stages
%% have finished and have not failed.
parent_stage_proceeds_to_next_stage_when_all_child_stages_are_successful(
        #deliv_stage{child_stage_pids=[Child1Pid, Child2Pid],
                     stage_run=StageRun,
                     change = Change,
                     consumer_change_ids=[ConsumerChangeId1, ConsumerChangeId2]} = State) ->

    ChangeId = deliv_change:getval(id, Change),
    PipeId = deliv_change:getval(pipeline_id, Change),
    ConsumerPipeId1 = fetch_pipe_id_by_change_id(ConsumerChangeId1),
    ConsumerPipeId2 = fetch_pipe_id_by_change_id(ConsumerChangeId2),

    {ok, RunningStageRun} = deliv_stage_run:update([{status, <<"running">>}], StageRun),
    ParentState = State#deliv_stage{next_phase_groups = [],
                                    stage_run = RunningStageRun,
                                    failed = not_failed_yet,
                                    passed_pipelines = [PipeId]},

    ExpectedFirstChildState =
        ParentState#deliv_stage{child_stage_pids = [Child2Pid],
                                passed_pipelines = [ConsumerPipeId1, PipeId]},

    {ok, FinishedStageRun} = deliv_stage_run:update([{finished, true}, {status, <<"passed">>}], RunningStageRun),
    ExpectedSecondChildState =
        ExpectedFirstChildState#deliv_stage{child_stage_pids = [],
                                            passed_pipelines = [ConsumerPipeId2, ConsumerPipeId1, PipeId],
                                            stage_run = FinishedStageRun},

    ExpectedEventBody = #{PipeId => passed, ConsumerPipeId1 => passed, ConsumerPipeId2 => passed},
    hoax:expect(receive
        %% Stub out initial_state to return some pre-determined values.
        deliv_scm_local:load_config_for_patchset(?any, ?any) -> {ok, []};
        %% Do not trigger next stage.
        deliverance_stage_queue:in(?any) -> ok;
        deliv_event:publish({{stage, finished}, union}, ?any) -> ignored;
        deliv_event:publish({build_event_for_change, ChangeId}, Change) -> ignored;
        deliv_event:publish(build_event_for_change, Change) -> ignored;
        deliv_event:publish(union_finished, ExpectedEventBody) -> ignored;
        deliv_stage_run:finish(RunningStageRun, passed) -> {ok, FinishedStageRun};
        deliv_stage_run:getval(stage, RunningStageRun) -> <<"build">>;
        deliv_stage_run:getval(change_id, RunningStageRun) -> ChangeId;
        % For testing, the return value doesn't matter as long as it is a #deliv_stage_run.
        deliv_stage_run:insert([{change_id, ChangeId}, {stage, <<"rehearsal">>}, {status, <<"idle">>}]) -> [StageRun]
              end),

    %% send stage_finished to a parent with multiple children
    Result1 = deliv_stage:running({child_stage_finished, Child1Pid, {ConsumerPipeId1, not_failed_yet}}, ParentState),
    ?assertEqual({next_state, running, ExpectedFirstChildState}, Result1),

    %% send another stage_finished for the second child
    Result2 = deliv_stage:running({child_stage_finished, Child2Pid, {ConsumerPipeId2, not_failed_yet}}, ExpectedFirstChildState),
    ?assertEqual({stop, normal, ExpectedSecondChildState}, Result2),
    ?verifyAll.

%% Deliv_stage sends a union_finished event when the union stage run is finished running
%% This test ensures that consumers who were not run are still included in the
%% list of affected pipelines.  This needs to happen since those pipelines were
%% part of the union run.
stage_with_two_consumers_when_parent_stage_fails_in_union_includes_consumer_pipeline_ids_in_union_finished_event(
        #deliv_stage{change=Change,
                     consumer_change_ids=[ConsumerChangeId1, ConsumerChangeId2],
                     stage_run=StageRun} = State) ->

    ChangeId = deliv_change:getval(id, Change),

    PipeId = deliv_change:getval(pipeline_id, Change),
    ConsumerPipeId1 = fetch_pipe_id_by_change_id(ConsumerChangeId1),
    ConsumerPipeId2 = fetch_pipe_id_by_change_id(ConsumerChangeId2),

    ParentState = State#deliv_stage{running_phases=[],
                                    next_phase_groups=[],
                                    failed=failed,
                                    failed_pipelines=[PipeId]},

    {ok, NewStageRun} = deliv_stage_run:update([{finished, true}], StageRun),
    ExpectedState = ParentState#deliv_stage{stage_run=NewStageRun},

    ExpectedEventBody = #{ConsumerPipeId1 => unknown, ConsumerPipeId2 => unknown, PipeId => failed},

    hoax:expect(receive
        deliv_event:publish({stage_run, failures}, ?any) -> ignored;
        deliv_event:publish({{stage, finished}, union}, ?any) -> ignored;
        deliv_event:publish({build_event_for_change, ChangeId}, Change) -> ignored;
        deliv_event:publish(build_event_for_change, Change) -> ignored;
        deliv_event:publish(union_finished, ExpectedEventBody) -> ignored;
        %% Stub out web-url for failure notification
        deliv_web_utils:make_web_url_for_change(?any, ?any, ?any, ?any) -> "http://change-url";

        deliv_stage_run:getval(stage, StageRun) -> <<"build">>;
        deliv_stage_run:getval(change_id, StageRun) -> ChangeId;
        deliv_stage_run:finish(StageRun, failed) -> {ok, NewStageRun}
                end),

    Result = deliv_stage:idle(start, ParentState),
    ?assertEqual({stop, normal, ExpectedState}, Result),
    ?verifyAll.

%% Ensure that a stage with no consumers which fails in union reports its
%% pipeline as a failed pipeline.
stage_with_no_consumers_added_to_failed_pipelines_on_failure(
        #deliv_stage{parent_stage_pid=ParentPid,
                     change=Change}=RawState) ->

    State = RawState#deliv_stage{running_phases=[]},

    PipeId = deliv_change:getval(pipeline_id, Change),

    %% Stub out web-url for failure notification
    hoax:mock(deliv_web_utils,
              ?expect(make_web_url_for_change,
                      ?withArgs([?any, ?any, ?any, ?any]),
                      ?andReturn("http://change-url"))),

    {stop, normal, #deliv_stage{failed_pipelines=FailedPipes}} =
        deliv_stage:running({phase_finished, ParentPid, failed}, State),
    ?assertEqual([PipeId], FailedPipes),
    ?verifyAll.

%% The stage has finished running all of its phases. We want to verify that
%% the pipeline id associated with the change is added to passed_pipelines
%% in the deliv_stage record when the stage has no consumers.
stage_with_no_consumers_adds_pipeline_id_to_passed_pipelines_on_success(
        #deliv_stage{patchset = Patchset,
                     scope = Scope,
                     project_config = ProjConfig,
                     change = Change} = State) ->

    State2 = State#deliv_stage{next_phase_groups = []},

    ChangeId = deliv_change:getval(id, Change),
    PipeId = deliv_change:getval(pipeline_id, Change),
    ExpectedEventBody = #{PipeId => passed},

    %% Stub out initial_state to return some pre-determined values
    hoax:mock(deliv_scm_local,
              ?expect(load_config_for_patchset,
                      ?withArgs([Patchset, Scope]),
                      ?andReturn({ok, ProjConfig}))),

    %% Do not trigger next stage.
    hoax:mock(deliverance_stage_queue,
              ?expect(in,
                      ?withArgs([?any]),
                      ?andReturn(ok))),

    hoax:mock(deliv_event,
        [
              ?expect(publish,
                      ?withArgs([{{stage, finished}, union}, ?any])),
              ?expect(publish,
                      ?withArgs([{build_event_for_change, ChangeId}, Change])),
              ?expect(publish,
                      ?withArgs([build_event_for_change, Change])),
              ?expect(publish,
                      ?withArgs([union_finished, ExpectedEventBody]))
        ]),

    {stop, normal, #deliv_stage{passed_pipelines=PassedPipes}} =
        deliv_stage:running({phase_finished, self(), passed}, State2),

    ?assertEqual([PipeId], PassedPipes),
    ?verifyAll.

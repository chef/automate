-module(deliv_hand_pipe_events_tests).

-include_lib("hoax/include/hoax.hrl").
-include("../../delivery/include/deliv_types.hrl").
-include("../../delivery/include/deliv_events.hrl").

-compile([export_all]).

fixture_test_() ->
    hoax:parameterized_fixture(?MODULE, format_phase, setup_phase, teardown).

setup_phase() ->
    StageRun = deliv_stage_run:'#new'(),
    PhaseRun = deliv_phase_run:'#new'(),
    Change = deliv_change:'#new'(),
    Patchset = deliv_patchset:'#new'(),

    PhaseEvent = #phase_event{
        stage_run = StageRun,
        phase_run = PhaseRun,
        change = Change,
        patchset = Patchset,
        job_status = <<"running">>
    },


    ChangeId = <<"ChangeId">>,
    StageName = <<"verify">>,
    StageRunId = 1,
    hoax:mock(deliv_stage_run, [
              ?expect(getval,
                      ?withArgs([change_id, StageRun]),
                      ?andReturn(ChangeId)),
              ?expect(getval,
                      ?withArgs([stage, StageRun]),
                      ?andReturn(StageName)),
              ?expect(getval,
                      ?withArgs([id, StageRun]),
                      ?andReturn(StageRunId))
    ]),

    EnterpriseName = <<"EntName">>,
    OrgName = <<"OrgName">>,
    ProjectName = <<"ProjName">>,
    PipelineName = <<"PipeName">>,
    hoax:mock(deliv_change,
              ?expect(scoping_names,
                      ?withArgs([ChangeId]),
                      ?andReturn([EnterpriseName, OrgName, ProjectName, PipelineName]))),

    PhaseName = <<"unit">>,
    PhaseRunId = 1,
    hoax:mock(deliv_phase_run, [
              ?expect(getval,
                      ?withArgs([phase, PhaseRun]),
                      ?andReturn(PhaseName)),
              ?expect(getval,
                      ?withArgs([id, PhaseRun]),
                      ?andReturn(PhaseRunId))
    ]),

    Timestamp = <<"Thu, 05 Feb 2015 17:27:28 GMT">>,
    hoax:mock(cowboy_clock,
              ?expect(rfc1123,
                      ?withArgs([]),
                      ?andReturn(Timestamp))),

    BaseMsg = [{<<"event_type">>, <<"pipeline">>},
               {<<"enterprise">>, EnterpriseName},
               {<<"organization">>, OrgName},
               {<<"project">>, ProjectName},
               {<<"pipeline">>, PipelineName},
               {<<"change_id">>, ChangeId},
               {<<"stage">>, StageName},
               {<<"stage_run_id">>, StageRunId},
               {<<"phase">>, PhaseName},
               {<<"phase_run_id">>, PhaseRunId},
               {<<"created_at">>, Timestamp}],

    {PhaseEvent, BaseMsg}.

teardown(_) ->
    ok.

assertPipelineSseReturnIncludesProps(Action, Props, BaseMsg, PhaseEvent) ->
    Msg = {ejson, {lists:flatten([BaseMsg | Props])}},
    hoax:mock(deliv_web_sse,
              ?expect(format_event,
                      ?withArgs([?any, <<"pipeline">>, Msg]),
                      ?andReturn(io_data))),

    Expected = {keep_open, req, state, io_data},
    Actual = deliv_hand_pipe_events:format_event({{phase, Action}, unit}, PhaseEvent, req, state),
    ?assertEqual(Expected, Actual),
    ?verifyAll.

events_returns_proper_subscription_keys_test() ->
    Expected = {req, state, [
        {orgs, crud},
        {{phase, started}, unit},
        {{phase, started}, lint},
        {{phase, started}, syntax},
        {{phase, started}, quality},
        {{phase, started}, security},
        {{phase, started}, publish},
        {{phase, started}, provision},
        {{phase, started}, deploy},
        {{phase, started}, smoke},
        {{phase, started}, functional},
        {{phase, updated}, unit},
        {{phase, updated}, lint},
        {{phase, updated}, syntax},
        {{phase, updated}, quality},
        {{phase, updated}, security},
        {{phase, updated}, publish},
        {{phase, updated}, provision},
        {{phase, updated}, deploy},
        {{phase, updated}, smoke},
        {{phase, updated}, functional},
        {{phase, finished}, unit},
        {{phase, finished}, lint},
        {{phase, finished}, syntax},
        {{phase, finished}, quality},
        {{phase, finished}, security},
        {{phase, finished}, publish},
        {{phase, finished}, provision},
        {{phase, finished}, deploy},
        {{phase, finished}, smoke},
        {{phase, finished}, functional}
    ]},
    Actual = deliv_hand_pipe_events:events(req, state),
    ?assertEqual(Expected, Actual).

format_phase_started_event_returns_sse_msg({#phase_event{phase_run = PhaseRun} = PhaseEvent, BaseMsg}) ->
    Node = <<"NodeName">>,
    hoax:mock(deliv_phase_run,
              ?expect(getval,
                      ?withArgs([build_node, PhaseRun]),
                      ?andReturn(Node))),

    EventProps = [{<<"event">>, <<"phase_started">>},
                  {<<"build_node">>, Node}],

    assertPipelineSseReturnIncludesProps(started, EventProps, BaseMsg, PhaseEvent).

format_phase_finished_event_due_to_skipping_retuns_sse_msg({#phase_event{} = PhaseEvent, BaseMsg}) ->
    EventProps = [{<<"event">>, <<"phase_completed">>},
                  {<<"status">>, <<"skipped">>}],
    assertPipelineSseReturnIncludesProps(finished, EventProps, BaseMsg, PhaseEvent#phase_event{status = skipped}).


format_phase_finished_event_due_to_no_worker_returns_sse_msg({#phase_event{} = PhaseEvent, BaseMsg}) ->
    EventProps = [{<<"event">>, <<"phase_completed">>},
                  {<<"status">>, <<"failed">>},
                  {<<"reason">>, <<"no available build nodes">>}],

    assertPipelineSseReturnIncludesProps(finished, EventProps, BaseMsg, PhaseEvent#phase_event{status = failed, status_reason = no_workers}).

format_phase_finished_event_due_to_job_being_canceled_by_user_returns_sse_msg({#phase_event{} = PhaseEvent, BaseMsg}) ->
    EventProps = [{<<"event">>, <<"phase_completed">>},
                  {<<"status">>, <<"failed">>},
                  {<<"reason">>, <<"job canceled by user">>}],

    assertPipelineSseReturnIncludesProps(finished, EventProps, BaseMsg, PhaseEvent#phase_event{status = failed, status_reason = job_canceled}).

format_phase_finished_event_due_to_status_returns_sse_msg({#phase_event{} = PhaseEvent, BaseMsg}) ->
    EventProps = [{<<"event">>, <<"phase_completed">>},
                  {<<"status">>, <<"passed">>}],
    assertPipelineSseReturnIncludesProps(finished, EventProps, BaseMsg, PhaseEvent#phase_event{status = passed}).

format_phase_updated_event_returns_sse_msg({#phase_event{job_status = JobStatus} = PhaseEvent, BaseMsg}) ->
    EventProps = [{<<"event">>, <<"phase_push_update">>},
                  {<<"status">>, JobStatus}],
    assertPipelineSseReturnIncludesProps(updated, EventProps, BaseMsg, PhaseEvent#phase_event{status = running}).

format_phase_waiting_for_worker_event_returns_sse_msg({#phase_event{} = PhaseEvent, BaseMsg}) ->
    EventProps = [{<<"event">>, <<"phase_await_worker">>}],
    assertPipelineSseReturnIncludesProps(waiting_for_worker, EventProps, BaseMsg, PhaseEvent).

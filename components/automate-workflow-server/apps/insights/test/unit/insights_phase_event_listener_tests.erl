-module(insights_phase_event_listener_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_events.hrl").

-include("insights.hrl").

-compile([export_all]).

handle_event_fixture_test_() ->
    hoax:parameterized_fixture(?MODULE, handle_event, setup, teardown).

setup() ->
    ChangeId = <<"ChangeId">>,
    StageName = <<"StageName">>,
    BuildNodeName = <<"deliv_phase_run::build_node">>,
    PhaseStatus = status,
    PhaseStatusBin = <<"status">>,
    PhaseStatusReason = status_reason,
    PhaseStatusReasonBin = <<"status_reason">>,
    ScopingNames = [<<"Chef">>, <<"Chef_Delivery">>, <<"delivery">>, <<"master">>],
    JobStatus = <<"push-job-status">>,

    StageRun = deliv_stage_run:'#new'(),
    PhaseRun = deliv_phase_run:'#new'(),
    Change = deliv_change:'#new'(),
    Patchset = deliv_patchset:'#new'(),

    PhaseEvent = #phase_event{
        stage_run = StageRun,
        phase_run = PhaseRun,
        change = Change,
        patchset = Patchset,
        status = PhaseStatus,
        status_reason = PhaseStatusReason,
        job_status = JobStatus
    },

    hoax:mock(deliv_change, [
              ?expect(getval,
                      ?withArgs([id, Change]),
                      ?andReturn(ChangeId)),
              ?expect(scoping_names,
                      ?withArgs([ChangeId]),
                      ?andReturn(ScopingNames))
                            ]),
    hoax:mock(deliv_stage_run,
              ?expect(getval,
                      ?withArgs([stage, StageRun]),
                      ?andReturn(StageName))),
    hoax:mock(deliv_phase_run, [
              ?expect(getval,
                      ?withArgs([build_node, PhaseRun]),
                      ?andReturn(BuildNodeName))
    ]),

    PhaseEjson = {[
        {<<"change_id">>, ChangeId},
        {<<"stage">>, StageName},
        {<<"phase">>, <<"unit">>},
        {<<"status">>, PhaseStatusBin},
        {<<"status_reason">>, PhaseStatusReasonBin},
        {<<"enterprise_name">>, <<"Chef">>},
        {<<"organization_name">>, <<"Chef_Delivery">>},
        {<<"pipeline_name">>, <<"master">>},
        {<<"project_name">>, <<"delivery">>},
        {<<"job_status">>, JobStatus},
        {<<"build_node">>, BuildNodeName}
    ]},

    {PhaseEvent, PhaseEjson, #insights_event{}}.

teardown(_) -> ok.

start_link_starts_listener_test() ->
    hoax:test(fun() ->
        hoax:mock(insights_listener,
                  ?expect(start_link,
                          ?withArgs([insights_phase_event_listener]))),
        insights_phase_event_listener:start_link(),
        ?verifyAll
    end).

subscribe_to_events_subscribes_to_phase_events_test() ->
    hoax:test(fun() ->
        hoax:mock(deliv_phase,
                  ?expect(subscribe_phase_events,
                          ?withArgs([]),
                          ?andReturn(true))),

        true = insights_phase_event_listener:subscribe_to_events(),
        ?verifyAll
    end).

handle_event_handles_phase_finished_event({PhaseEvent, PhaseEjson, InsightsEvent}) ->
    hoax:mock(insights_listener,
              ?expect(new_event,
                      ?withArgs([phase, finished, PhaseEjson]),
                      ?andReturn(InsightsEvent))),

    Actual = insights_phase_event_listener:handle_event({{phase, finished}, unit}, PhaseEvent),
    ?assertEqual(InsightsEvent, Actual),
    ?verifyAll.

handle_event_handles_phase_waiting_for_worker_event({PhaseEvent, PhaseEjson, InsightsEvent}) ->
    hoax:mock(insights_listener,
              ?expect(new_event,
                      ?withArgs([phase, waiting_for_worker, PhaseEjson]),
                      ?andReturn(InsightsEvent))),

    Actual = insights_phase_event_listener:handle_event({{phase, waiting_for_worker}, unit}, PhaseEvent),
    ?assertEqual(InsightsEvent, Actual),
    ?verifyAll.

handle_event_handles_phase_started_event({PhaseEvent, PhaseEjson, InsightsEvent}) ->
    hoax:mock(insights_listener,
              ?expect(new_event,
                      ?withArgs([phase, started, PhaseEjson]),
                      ?andReturn(InsightsEvent))),

    Actual = insights_phase_event_listener:handle_event({{phase, started}, unit}, PhaseEvent),
    ?assertEqual(InsightsEvent, Actual),
    ?verifyAll.

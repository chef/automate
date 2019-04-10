-module(deliv_phase_event_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("../../src/deliv_phase.hrl").

-compile([export_all]).

new_returns_phase_event_test() ->
    PhaseBin = <<"unit">>,
    PhaseAtom = unit,

    StageRun = deliv_stage_run:'#new'(),
    PhaseRun = deliv_phase_run:fromlist([{phase, PhaseBin}]),
    Change = deliv_change:'#new'(),
    Patchset = deliv_patchset:'#new'(),
    Status = finished,
    Reason = no_workers,
    JobStatus = <<"job_status">>,

    PhaseState = #deliv_phase{
        stage_run = StageRun,
        phase_run = PhaseRun,
        change = Change,
        patchset = Patchset
    },

    Expected = #phase_event{
        phase_name = PhaseAtom,
        stage_run = StageRun,
        phase_run = PhaseRun,
        change = Change,
        patchset = Patchset,
        status = Status,
        status_reason = Reason,
        job_status = JobStatus
    },
    Actual = deliv_phase_event:new(Status, Reason, JobStatus, PhaseState),
    ?assertEqual(Expected, Actual).

publish_calls_deliv_event_test() ->
    hoax:test(fun() ->
        PhaseAtom = unit,
        Action = waiting_for_worker,
        PhaseEvent = #phase_event{phase_name = PhaseAtom},
        hoax:mock(deliv_event,
                  ?expect(publish,
                          ?withArgs([{{phase, Action}, PhaseAtom}, PhaseEvent]),
                          ?andReturn(PhaseEvent))),

        Expected = PhaseEvent,
        Actual = deliv_phase_event:publish(Action, PhaseEvent),
        ?assertEqual(Expected, Actual),
        ?verifyAll
    end).

%%
%% subscribe_to_events
%%
subscribe_fixture_test_() ->
    hoax:fixture(?MODULE, subscribe).

subscribe_all_actions_all_phases() ->
    hoax:mock(deliv_event,
              ?expect(subscribe,
                     ?withArgs([[
                               {{phase, waiting_for_worker}, unit},
                               {{phase, finished}, unit},
                               {{phase, updated}, unit},
                               {{phase, started}, unit},
                               {{phase, waiting_for_worker}, lint},
                               {{phase, finished}, lint},
                               {{phase, updated}, lint},
                               {{phase, started}, lint},
                               {{phase, waiting_for_worker}, syntax},
                               {{phase, finished}, syntax},
                               {{phase, updated}, syntax},
                               {{phase, started}, syntax},
                               {{phase, waiting_for_worker}, quality},
                               {{phase, finished}, quality},
                               {{phase, updated}, quality},
                               {{phase, started}, quality},
                               {{phase, waiting_for_worker}, security},
                               {{phase, finished}, security},
                               {{phase, updated}, security},
                               {{phase, started}, security},
                               {{phase, waiting_for_worker}, publish},
                               {{phase, finished}, publish},
                               {{phase, updated}, publish},
                               {{phase, started}, publish},
                               {{phase, waiting_for_worker}, provision},
                               {{phase, finished}, provision},
                               {{phase, updated}, provision},
                               {{phase, started}, provision},
                               {{phase, waiting_for_worker}, deploy},
                               {{phase, finished}, deploy},
                               {{phase, updated}, deploy},
                               {{phase, started}, deploy},
                               {{phase, waiting_for_worker}, smoke},
                               {{phase, finished}, smoke},
                               {{phase, updated}, smoke},
                               {{phase, started}, smoke},
                               {{phase, waiting_for_worker}, functional},
                               {{phase, finished}, functional},
                               {{phase, updated}, functional},
                               {{phase, started}, functional}
                               ]]))),

    deliv_phase_event:subscribe(),
    ?verifyAll.

subscribe_specific_action_all_phases() ->
    hoax:mock(deliv_event,
              ?expect(subscribe,
                      ?withArgs([[
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
                      ]]))),
    deliv_phase_event:subscribe(finished, all),
    ?verifyAll.

subscribe_all_actions_specific_phase() ->
    hoax:mock(deliv_event,
              ?expect(subscribe,
                      ?withArgs([[
                        {{phase, started}, unit},
                        {{phase, updated}, unit},
                        {{phase, finished}, unit},
                        {{phase, waiting_for_worker}, unit}
                      ]]))),
    deliv_phase_event:subscribe(all, unit),
    ?verifyAll.

subscribe_specific_action_specific_phase() ->
    hoax:mock(deliv_event,
              ?expect(subscribe,
                      ?withArgs([[
                        {{phase, started}, unit}
                      ]]))),
    deliv_phase_event:subscribe(started, unit),
    ?verifyAll.

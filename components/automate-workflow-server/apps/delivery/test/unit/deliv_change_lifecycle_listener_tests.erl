-module(deliv_change_lifecycle_listener_tests).

-include_lib("hoax/include/hoax.hrl").
-include("../../include/deliv_types.hrl").
-include("../../include/deliv_events.hrl").

-compile([export_all]).

fixture_test_() ->
    hoax:fixture(?MODULE).

subscribes_to_patchset_created_events() ->
    hoax:mock(deliv_patchset,
              ?expect(subscribe_patchset_events,
                      ?withArgs([]),
                      ?andReturn(true))),
   hoax:mock(deliv_phase,
             ?expect(subscribe_phase_events,
                     ?withArgs([started, all]),
                     ?andReturn(true))),

    Actual = deliv_change_lifecycle_listener:init([]),
    ?assertEqual({ok, stateless}, Actual),
    ?verifyAll.


handle_info_triggers_change_created_event_when_receives_patchset_created_event() ->
    Patchset = deliv_patchset:'#new'(),
    Change = deliv_change:'#new'(),
    ChangeId = <<"ChangeGuid">>,

    hoax:mock(deliv_patchset,[
              ?expect(getval, ?withArgs([change_id, Patchset]), ?andReturn(ChangeId)),
              ?expect(getval, ?withArgs([sequence_number, Patchset]), ?andReturn(1))
    ]),

    hoax:mock(deliv_change,
              ?expect(fetch_by_id,
                      ?withArgs([ChangeId]),
                      ?andReturn({ok, Change}))),

    hoax:mock(deliv_event,
              ?expect(publish,
                      ?withArgs([change_created, Change]))),

    Actual = deliv_change_lifecycle_listener:handle_info({self(), patchset_created, Patchset}, stateless),
    ?assertEqual({noreply, stateless}, Actual),
    ?verifyAll.

handle_info_does_not_trigger_change_created_event_when_the_patchset_sequence_number_is_not_one() ->
    Patchset = deliv_patchset:'#new'(),

    hoax:mock(deliv_patchset,[
              ?expect(getval, ?withArgs([sequence_number, Patchset]), ?andReturn(2))
    ]),

    hoax:mock(deliv_event,
              ?expect(publish,
                      ?withArgs([?any, ?any]),
                      ?times(0))),

    Actual = deliv_change_lifecycle_listener:handle_info({self(), patchset_created, Patchset}, stateless),
    ?assertEqual({noreply, stateless}, Actual),
    ?verifyAll.

handle_info_triggers_build_event_for_change_when_phase_run_starts() ->
    ChangeId = <<"ChangeId">>,
    Change = deliv_change:fromlist([{id, ChangeId}]),
    PhaseEvent = #phase_event{change = Change},

    hoax:mock(deliv_change,
              ?expect(getval,
                      ?withArgs([id, Change]),
                      ?andReturn(ChangeId))),
    hoax:mock(deliv_event, [
              ?expect(publish,
                      ?withArgs([build_event_for_change, Change]),
                      ?times(1)),
              ?expect(publish,
                      ?withArgs([{build_event_for_change, ChangeId}, Change]),
                      ?times(1))
    ]),
    Actual = deliv_change_lifecycle_listener:handle_info({self(), {{phase, started}, unit}, PhaseEvent}, stateless),
    ?assertEqual({noreply, stateless}, Actual),
    ?verifyAll.

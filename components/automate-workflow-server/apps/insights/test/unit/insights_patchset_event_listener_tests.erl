-module(insights_patchset_event_listener_tests).

-include_lib("hoax/include/hoax.hrl").
-include("insights.hrl").

-compile([export_all]).

fixture_test_() ->
    hoax:fixture(?MODULE).

start_link_starts_listener() ->
    hoax:mock(insights_listener,
              ?expect(start_link,
                      ?withArgs([insights_patchset_event_listener]))),
    insights_patchset_event_listener:start_link(),
    ?verifyAll.

subscribe_to_events_subscribes_to_patchset_events() ->
    hoax:mock(deliv_patchset,
              ?expect(subscribe_patchset_events,
                      ?withArgs([]),
                      ?andReturn(ok))),

    ok = insights_patchset_event_listener:subscribe_to_events(),
    ?verifyAll.

handle_event_handles_patchset_created_events() ->
    PatchsetEvent = deliv_patchset:fromlist([]),
    SHA = <<"2841ca3c37dca5b591f43078407403820f23d328">>,
    SubmittedAt = {{2016, 01, 28}, {1, 33, 22}},
    DiffstatsJson = [{}],
    ChangedFilesJson = [{}],
    CommitsJson = [[]],
    PatchsetId = 48,
    IsVerified = true,
    SequenceNumber = 1,
    Status = failed,
    SubmitterId = 66,
    ChangeId = <<"0de0e404-be81-4fe0-9822-9650dbdcdb44">>,

    PatchsetToEjson = {[
        {<<"sequence_number">>, 1},
        {<<"sha">>, SHA},
        {<<"submitted_at">>, SubmittedAt},
        {<<"stats">>, DiffstatsJson},
        {<<"files_changed">>, ChangedFilesJson},
        {<<"commit_msgs">>, CommitsJson}
    ]},
    Ejson = {[
        {<<"change_id">>, ChangeId},
        {<<"commit_msgs">>, CommitsJson},
        {<<"files_changed">>, ChangedFilesJson},
        {<<"is_verified">>, IsVerified},
        {<<"patchset_id">>, PatchsetId},
        {<<"sequence_number">>, SequenceNumber},
        {<<"sha">>, SHA},
        {<<"stats">>, DiffstatsJson},
        {<<"status">>, Status},
        {<<"submitted_at">>, SubmittedAt},
        {<<"submitter_id">>, SubmitterId},
        {<<"verified_against_sha">>, SHA}
    ]},

    Scope = deliv_scopes:'#new_common'(),

    Event = #insights_event{},
    hoax:mock(insights_listener,
              ?expect(new_event,
                      ?withArgs([patchset, created, Ejson]),
                      ?andReturn(Event))),

    hoax:mock(deliv_patchset, [
        ?expect(to_ejson, ?withArgs([Scope, PatchsetEvent]), ?andReturn({ok, PatchsetToEjson})),
        ?expect(getval, ?withArgs([id, PatchsetEvent]), ?andReturn(PatchsetId)),
        ?expect(getval, ?withArgs([change_id, PatchsetEvent]), ?andReturn(ChangeId)),
        ?expect(getval, ?withArgs([sequence_number, PatchsetEvent]), ?andReturn(SequenceNumber)),
        ?expect(getval, ?withArgs([submitted_at, PatchsetEvent]), ?andReturn(SubmittedAt)),
        ?expect(getval, ?withArgs([sha, PatchsetEvent]), ?andReturn(SHA)),
        ?expect(getval, ?withArgs([submitter_id, PatchsetEvent]), ?andReturn(SubmitterId)),
        ?expect(getval, ?withArgs([verified_against_sha, PatchsetEvent]), ?andReturn(SHA)),
        ?expect(getval, ?withArgs([status, PatchsetEvent]), ?andReturn(Status)),
        ?expect(getval, ?withArgs([is_verified, PatchsetEvent]), ?andReturn(IsVerified))
    ]),

    hoax:mock(deliv_scopes,
              ?expect(from_patchset,
                      ?withArgs([PatchsetEvent]),
                      ?andReturn(Scope))),

    Actual = insights_patchset_event_listener:handle_event(patchset_created, PatchsetEvent),

    ?assertEqual(Event, Actual),
    ?verifyAll.

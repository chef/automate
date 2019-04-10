-module(insights_change_event_listener_tests).

-include_lib("hoax/include/hoax.hrl").

-include("insights.hrl").

-compile([export_all]).

handle_event_fixture_test_() ->
  hoax:parameterized_fixture(?MODULE, handle_change, setup, teardown).


setup() ->
    ChangeId = <<"ChangeGuid">>,
    PipelineId = 1,
    FeatureBranch = <<"FeatureBranch">>,
    MergeSha = <<"MergeSha">>,
    Title = <<"Title">>,
    Description = <<"Description">>,
    ApprovedAt = {{2016, 01, 28}, {2, 33, 22}},
    ApprovedAtText = <<"2016-01-28 02:33:22">>,
    ApprovedBy = <<"approver_name">>,
    ChangesetId = <<"ChangesetGuid">>,
    LatestPatchsetStatus = <<"merged">>,
    LatestPatchset = 1,
    ScopingNames = [<<"Chef">>, <<"Chef_Delivery">>, <<"delivery">>, <<"master">>],
    SubmittedAt = {{2016, 01, 28}, {1, 33, 22}},
    SubmittedAtText = <<"2016-01-28 01:33:22">>,
    SubmittedBy = <<"submitter_name">>,
    DeliveredAt = {{2016, 01, 28}, {1, 44, 10}},
    DeliveredAtText = <<"2016-01-28 01:44:10">>,
    DeliveredBy = <<"shipper_name">>,
    PipeNameAtCreation = <<"master">>,
    SupersedingChangeId = undefined,

    Change = deliv_change:'#new'(),
    hoax:mock(deliv_change, [
        ?expect(getval, ?withArgs([id, Change]), ?andReturn(ChangeId)),
        ?expect(getval, ?withArgs([feature_branch, Change]), ?andReturn(FeatureBranch)),
        ?expect(getval, ?withArgs([pipeline_name_at_creation, Change]), ?andReturn(PipeNameAtCreation)),
        ?expect(getval, ?withArgs([latest_patchset_status, Change]), ?andReturn(LatestPatchsetStatus)),
        ?expect(getval, ?withArgs([submitted_at, Change]), ?andReturn(SubmittedAt)),
        ?expect(getval, ?withArgs([submitted_by, Change]), ?andReturn(SubmittedBy)),
        ?expect(getval, ?withArgs([merge_sha, Change]), ?andReturn(MergeSha)),
        ?expect(getval, ?withArgs([approved_at, Change]), ?andReturn(ApprovedAt)),
        ?expect(getval, ?withArgs([approved_by, Change]), ?andReturn(ApprovedBy)),
        ?expect(getval, ?withArgs([title, Change]), ?andReturn(Title)),
        ?expect(getval, ?withArgs([description, Change]), ?andReturn(Description)),
        ?expect(getval, ?withArgs([delivered_by, Change]), ?andReturn(DeliveredBy)),
        ?expect(getval, ?withArgs([delivered_at, Change]), ?andReturn(DeliveredAt)),
        ?expect(getval, ?withArgs([changeset_id, Change]), ?andReturn(ChangesetId)),
        ?expect(getval, ?withArgs([latest_patchset, Change]), ?andReturn(LatestPatchset)),
        ?expect(getval, ?withArgs([pipeline_id, Change]), ?andReturn(PipelineId)),
        ?expect(getval, ?withArgs([superseding_change_id, Change]), ?andReturn(SupersedingChangeId)),
        ?expect(scoping_names, ?withArgs([ChangeId]), ?andReturn(ScopingNames))
    ]),

    ChangeEjson = {[
        {<<"change_id">>, ChangeId},
        {<<"topic">>, FeatureBranch},
        {<<"target">>, PipeNameAtCreation},
        {<<"state">>, LatestPatchsetStatus},
        {<<"enterprise_name">>, <<"Chef">>},
        {<<"organization_name">>, <<"Chef_Delivery">>},
        {<<"pipeline_name">>, <<"master">>},
        {<<"project_name">>, <<"delivery">>},
        {<<"submitted_at">>, SubmittedAtText},
        {<<"submitted_by">>, SubmittedBy},
        {<<"merge_sha">>, MergeSha},
        {<<"approved_by">>, ApprovedBy},
        {<<"approved_at">>, ApprovedAtText},
        {<<"title">>, Title},
        {<<"description">>, Description},
        {<<"delivered_by">>, DeliveredBy},
        {<<"delivered_at">>, DeliveredAtText},
        {<<"changeset_id">>, ChangesetId},
        {<<"latest_patchset">>, LatestPatchset},
        {<<"pipeline_id">>, PipelineId},
        {<<"superseding_change_id">>, SupersedingChangeId}
    ]},

    {Change, ChangeEjson, #insights_event{}}.

teardown(_) -> ok.

start_link_starts_listener() ->
    hoax:test(fun() ->
        hoax:mock(insights_listener,
                  ?expect(start_link,
                          ?withArgs([insights_change_event_listener]))),
        insights_change_event_listener:start_link(),
        ?verifyAll
    end).

subscribe_to_change_events() ->
    hoax:test(fun() ->
        hoax:mock(deliv_change,
                  ?expect(subscribe_change_events,
                          ?withArgs([]),
                          ?andReturn(ok))),

        ok = insights_change_event_listener:subscribe_to_events(),
        ?verifyAll
    end).

handle_change_created_event({Change, ChangeEjson, Event}) ->
    hoax:mock(insights_listener,
              ?expect(new_event,
                      ?withArgs([change, created, ChangeEjson]),
                      ?andReturn(Event))),

    Actual = insights_change_event_listener:handle_event(change_created, Change),

    ?assertEqual(Event, Actual),
    ?verifyAll.

handle_change_updated_event({Change, ChangeEjson, Event}) ->
    hoax:mock(insights_listener,
              ?expect(new_event,
                      ?withArgs([change, updated, ChangeEjson]),
                      ?andReturn(Event))),

    Actual = insights_change_event_listener:handle_event(change_updated, Change),

    ?assertEqual(Event, Actual),
    ?verifyAll.

handle_change_approved_event({Change, ChangeEjson, Event}) ->
    hoax:mock(insights_listener,
              ?expect(new_event,
                      ?withArgs([change, approved, ChangeEjson]),
                      ?andReturn(Event))),

    Actual = insights_change_event_listener:handle_event(change_approved, Change),

    ?assertEqual(Event, Actual),
    ?verifyAll.

handle_change_superseded_event({Change, ChangeEjson, Event}) ->
    hoax:mock(insights_listener,
              ?expect(new_event,
                      ?withArgs([change, superseded, ChangeEjson]),
                      ?andReturn(Event))),

    Actual = insights_change_event_listener:handle_event(change_superseded, Change),

    ?assertEqual(Event, Actual),
    ?verifyAll.

handle_change_delivered_event({Change, ChangeEjson, Event}) ->
    hoax:mock(insights_listener,
              ?expect(new_event,
                      ?withArgs([change, delivered, ChangeEjson]),
                      ?andReturn(Event))),

    Actual = insights_change_event_listener:handle_event(change_delivered, Change),

    ?assertEqual(Event, Actual),
    ?verifyAll.

handle_change_deleted_event() ->
    hoax:test(fun() ->
        ChangeId = <<"ChangeId">>,
        DeleterUsername = <<"bob">>,
        Deleter = deliv_user:'#new'(),

        ChangeEjson = {[
            {<<"change_id">>, ChangeId},
            {<<"deleted_by">>, DeleterUsername},
            {<<"enterprise_name">>, <<"Chef">>},
            {<<"organization_name">>, <<"Chef_Delivery">>},
            {<<"pipeline_name">>, <<"master">>},
            {<<"project_name">>, <<"delivery">>}
        ]},

        Event = #insights_event{},
        hoax:mock(insights_listener,
                  ?expect(new_event,
                          ?withArgs([change, deleted, ChangeEjson]),
                          ?andReturn(Event))),
        hoax:mock(deliv_user,
                  ?expect(getval,
                          ?withArgs([name, Deleter]),
                          ?andReturn(DeleterUsername))),

        Actual = insights_change_event_listener:handle_event(change_deleted, {{change_id, ChangeId}, {deleted_by, Deleter}}),

        ?assertEqual(Event, Actual),
        ?verifyAll
    end).

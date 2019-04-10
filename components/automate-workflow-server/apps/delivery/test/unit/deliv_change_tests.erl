-module(deliv_change_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_types.hrl").

-compile(export_all).

hoax_fixture_test_ () ->
    hoax:fixture(?MODULE).

update_publishes_change_updated_events() ->
    ChangeId = <<"ChangeId">>,
    UpdatedChange = Change = deliv_change:fromlist([{id, ChangeId}]),

    hoax:mock(deliv_event, [
              ?expect(publish,
                      ?withArgs([change_updated, UpdatedChange])),
              ?expect(publish,
                      ?withArgs([{change_updated, ChangeId}, UpdatedChange]))
    ]),

    hoax:mock(deliv_db,
              ?expect(update,
                      ?withArgs([Change]),
                      ?andReturn({ok, UpdatedChange}))),

    Result = deliv_change:update(Change),
    Expected = {ok, UpdatedChange},

    ?assertEqual(Expected, Result),
    ?verifyAll.

merge_unknown_change_returns_error() ->
    ChangeId = <<"fakeChangeId">>,
    Approver = deliv_user:'#new'(),
    hoax:mock(deliv_db,
              ?expect(fetch_by_id,
                      ?withArgs([deliv_change, ChangeId]),
                      ?andReturn({error, not_found}))),
    Result = deliv_change:merge(ChangeId, Approver),
    ?assertEqual({error, change_not_found}, Result).

merge_returns_error_if_scm_merge_fails() ->
    ChangeId = <<"mockedChangeId">>,
    PatchsetId = <<"mockedPatchsetId">>,
    PipelineId = <<"mockedPipelineId">>,
    MergeSha = <<"mockedMergeSha">>,
    ApproverName = <<"Approver">>,
    Approver = deliv_user:fromlist([{name, ApproverName}]),
    Change = deliv_change:fromlist([{id, ChangeId},
                                    {pipeline_id, PipelineId},
                                    {latest_patchset, PatchsetId},
                                    {latest_patchset_status, <<"open">>}]),
    UpdatedChange = deliv_change:setvals([{approved_by, Approver}, {merge_sha, MergeSha}], Change),
    Patchset = deliv_patchset:fromlist([{status, <<"open">>}]),
    ScmMod = deliv_scm_mock,
    Scope = deliv_scopes:'#new_common'([{scm_module, ScmMod}]),
    ScmMergeError = {error, merge_error},

    hoax:mock(deliv_patchset,
              ?expect(latest_patchset_for_change,
                      ?withArgs([ChangeId]),
                      ?andReturn({ok, Patchset}))),
    hoax:mock(deliv_db, [
                         ?expect(fetch_by_id,
                                 ?withArgs([deliv_change, ChangeId]),
                                 ?andReturn({ok, Change})),
                         ?expect(qfetch,
                                 ?withArgs([deliv_change, update_change_for_merge, [ChangeId, MergeSha, ApproverName]]),
                                 ?times(0)),
                         ?expect(select,
                                 ?withArgs([deliv_change, get_stage_run_summary, [ChangeId], rows_as_records, [stage_run_summary, [id,stage,status,finished,pipeline_latest,system_latest]]]),
                                 ?andReturn({ok, [#stage_run_summary{stage = <<"verify">>,
                                                                     status = <<"passed">>,
                                                                     finished = true}]}))
                        ]),
    hoax:mock(deliv_event, [
              ?expect(publish,
                      ?withArgs([{change_approved, ChangeId}, UpdatedChange]),
                      ?times(0)),
              ?expect(publish,
                      ?withArgs([change_approved, UpdatedChange]),
                      ?times(0))

    ]),
    hoax:mock(deliv_stage,
              ?expect(trigger,
                      ?withArgs([build, UpdatedChange]),
                      ?times(0))),
    hoax:mock(deliv_scopes, [
              ?expect(from_change,
                      ?withArgs([Change]),
                      ?andReturn(Scope)),
              ?expect('#get',
                      ?withArgs([[scm_module], Scope]),
                      ?andReturn([ScmMod]))

    ]),
    hoax:mock_behaviour(deliv_scm, ScmMod, [
                                ?expect(merge_feature_branch,
                                        ?withArgs([Scope, Patchset, Approver]),
                                        ?andReturn(ScmMergeError))
                               ]),
    Result = deliv_change:merge(ChangeId, Approver),
    ?assertEqual(ScmMergeError, Result),
    ?verifyAll.

merge_returns_error_if_db_update_fails() ->
    ChangeId = <<"mockedChangeId">>,
    PipelineId = <<"mockedPipelineId">>,
    MergeSha = <<"mockedMergeSha">>,
    ApproverName = <<"Approver">>,
    Approver = deliv_user:fromlist([{name, ApproverName}]),
    Change = deliv_change:fromlist([{id, ChangeId},
                                    {pipeline_id, PipelineId},
                                    {latest_patchset_status, <<"open">>}]),
    UpdatedChange = deliv_change:setvals([{approved_by, Approver}, {merge_sha, MergeSha}], Change),
    Patchset = deliv_patchset:fromlist([{status, <<"open">>}]),
    ScmMod = deliv_scm_mock,
    Scope = deliv_scopes:'#new_common'([{scm_module, ScmMod}]),
    DbUpdateMergeError = {error, db_update_failure},

    hoax:mock(deliv_patchset,
              ?expect(latest_patchset_for_change,
                      ?withArgs([ChangeId]),
                      ?andReturn({ok, Patchset}))),
    hoax:mock(deliv_db, [
                         ?expect(fetch_by_id,
                                 ?withArgs([deliv_change, ChangeId]),
                                 ?andReturn({ok, Change})),
                         ?expect(qfetch,
                                 ?withArgs([deliv_change, update_change_for_merge, [ChangeId, MergeSha, ApproverName]]),
                                 ?andReturn(DbUpdateMergeError)),
                         ?expect(select,
                                 ?withArgs([deliv_change, get_stage_run_summary, [ChangeId], rows_as_records, [stage_run_summary, [id,stage,status,finished,pipeline_latest,system_latest]]]),
                                 ?andReturn({ok, [#stage_run_summary{stage = <<"verify">>,
                                                                     status = <<"passed">>,
                                                                     finished = true}]}))
                        ]),
    hoax:mock(deliv_event, [
              ?expect(publish,
                      ?withArgs([{change_approved, ChangeId}, UpdatedChange]),
                      ?times(0)),
              ?expect(publish,
                      ?withArgs([change_approved, UpdatedChange]),
                      ?times(0))

    ]),
    hoax:mock(deliv_stage,
              ?expect(trigger,
                      ?withArgs([build, UpdatedChange]),
                      ?times(0))),
    hoax:mock(deliv_scopes, [
              ?expect(from_change,
                      ?withArgs([Change]),
                      ?andReturn(Scope)),
              ?expect('#get',
                      ?withArgs([[scm_module], Scope]),
                      ?andReturn([ScmMod]))

    ]),
    hoax:mock_behaviour(deliv_scm, ScmMod, [
                                ?expect(merge_feature_branch,
                                        ?withArgs([Scope, Patchset, Approver]),
                                        ?andReturn({ok, MergeSha})),
                                ?expect(delete_feature_branch,
                                        ?withArgs([Scope, Patchset]),
                                        ?times(0))
                               ]),
    Result = deliv_change:merge(ChangeId, Approver),
    ?assertEqual(DbUpdateMergeError, Result),
    ?verifyAll.

merge_unopen_change_returns_error() ->
    ChangeId = <<"mockedChangeId">>,
    PipelineId = <<"mockedPipelineId">>,
    PatchsetStatus = <<"latestpatchset">>,
    Change = deliv_change:fromlist([{id, ChangeId},
                                    {pipeline_id, PipelineId},
                                    {latest_patchset_status, PatchsetStatus}]),
    Pipeline = deliv_pipeline:'#new'(),
    Patchset = deliv_patchset:fromlist([{status, PatchsetStatus}]),
    Approver = deliv_user:'#new'(),
    hoax:mock(deliv_patchset,
              ?expect(latest_patchset_for_change,
                      ?withArgs([ChangeId]),
                      ?andReturn({ok, Patchset}))),
    hoax:mock(deliv_db,
              ?expect(fetch_by_id,
                      ?withArgs([deliv_change, ChangeId]),
                      ?andReturn({ok, Change}))),
    hoax:mock(deliv_db,
              ?expect(fetch_by_id,
                      ?withArgs([deliv_pipeline, PipelineId]),
                      ?andReturn({ok, Pipeline}))),
    Result = deliv_change:merge(ChangeId, Approver),
    ?assertEqual({error, patchset_already_merged}, Result).

merge_open_change_succeeds() ->
    ChangeId = <<"mockedChangeId">>,
    PipelineId = <<"mockedPipelineId">>,
    MergeSha = <<"mockedMergeSha">>,
    ApproverName = <<"Approver">>,
    Approver = deliv_user:fromlist([{name, ApproverName}]),
    Change = deliv_change:fromlist([{id, ChangeId},
                                    {pipeline_id, PipelineId},
                                    {latest_patchset_status, <<"open">>}]),
    UpdatedChange = deliv_change:setvals([{approved_by, Approver}, {merge_sha, MergeSha}], Change),
    Patchset = deliv_patchset:fromlist([{status, <<"open">>}]),
    ScmMod = deliv_scm_mock,
    Scope = deliv_scopes:'#new_common'([{scm_module, ScmMod}]),
    hoax:mock(deliv_patchset,
              ?expect(latest_patchset_for_change,
                      ?withArgs([ChangeId]),
                      ?andReturn({ok, Patchset}))),
    hoax:mock(deliv_db, [
                         ?expect(fetch_by_id,
                                 ?withArgs([deliv_change, ChangeId]),
                                 ?andReturn({ok, Change})),
                         ?expect(qfetch,
                                 ?withArgs([deliv_change, update_change_for_merge, [ChangeId, MergeSha, ApproverName]]),
                                 ?andReturn([UpdatedChange])),
                         ?expect(select,
                                 ?withArgs([deliv_change, get_stage_run_summary, [ChangeId], rows_as_records, [stage_run_summary, [id,stage,status,finished,pipeline_latest,system_latest]]]),
                                 ?andReturn({ok, [#stage_run_summary{stage = <<"verify">>,
                                                                     status = <<"passed">>,
                                                                     finished = true}]}))
                        ]),
    hoax:mock(deliv_event, [
              ?expect(publish,
                      ?withArgs([{change_approved, ChangeId}, UpdatedChange])),
              ?expect(publish,
                      ?withArgs([change_approved, UpdatedChange]))

    ]),
    hoax:mock(deliv_patchset_changed_file,
              ?expect(refresh_cache,
                      ?withArgs([Scope, UpdatedChange, Patchset]))),
    hoax:mock(deliv_stage,
              ?expect(trigger,
                      ?withArgs([build, UpdatedChange]),
                      ?andReturn(ok))),
    hoax:mock(deliv_scopes, [
              ?expect(from_change,
                      ?withArgs([Change]),
                      ?andReturn(Scope)),
              ?expect('#get',
                      ?withArgs([[scm_module], Scope]),
                      ?andReturn([ScmMod]))

    ]),
    hoax:mock_behaviour(deliv_scm, ScmMod, [
                                ?expect(merge_feature_branch,
                                        ?withArgs([Scope, Patchset, Approver]),
                                        ?andReturn({ok, MergeSha})),
                                ?expect(delete_feature_branch,
                                        ?withArgs([Scope, Patchset]),
                                        ?andReturn({ok, ignored}))
                               ]),
    Result = deliv_change:merge(ChangeId, Approver),
    ?assertEqual(ok, Result),
    ?verifyAll.

get_superseding_change_returns_superseding_change() ->
    ChangeId = <<"change-id">>,
    SupersedingChangeId = <<"superseding-id">>,
    Change = deliv_change:'#new'(),
    Change2 = deliv_change:setvals([{id, ChangeId}, {superseding_change_id, SupersedingChangeId}], Change),
    SupersedingChange = deliv_change:'#new'(),
    SupersedingChange2 = deliv_change:setvals([{id, SupersedingChangeId}], SupersedingChange),

    hoax:mock(deliv_db, [
              ?expect(fetch_by_id,
                      ?withArgs([deliv_change, SupersedingChangeId]),
                      ?andReturn({ok, SupersedingChange2}))]),

    Result = deliv_change:get_superseding_change(Change2),

    ?assertEqual({ok, SupersedingChange2}, Result),
    ?verifyAll.

set_superseded_changes_returns_superseded_changes() ->
    Change1Id = <<"aaa-bbb-ccc-ddd">>,
    Change2Id = <<"eee-fff-ggg-hhh">>,
    SupersedingChangeId = <<"www-xxx-yyy-zzz">>,

    Change1 = deliv_change:fromlist([{id, Change1Id}]),
    Change2 = deliv_change:fromlist([{id, Change2Id}]),
    SupersedingChange = deliv_change:fromlist([{id, SupersedingChangeId}]),

    SupersededChanges = [Change1, Change2],

    hoax:mock(deliv_db,
              ?expect(qfetch,
                      ?withArgs([deliv_change, set_superseded_changes, [SupersedingChangeId]]),
                      ?andReturn(SupersededChanges))),

    hoax:mock(deliv_event,
        [
              ?expect(publish,
                      ?withArgs([{change_superseded, Change1Id}, Change1])),
              ?expect(publish,
                      ?withArgs([change_superseded, Change1])),
              ?expect(publish,
                      ?withArgs([{change_superseded, Change2Id}, Change2])),
              ?expect(publish,
                      ?withArgs([change_superseded, Change2]))
        ]),

    Result = deliv_change:set_superseded_changes(SupersedingChange),
    ?verifyAll,
    ?assertEqual(SupersededChanges, Result).

status_returns_delivered_if_delivered() ->
    Change = deliv_change:fromlist([{delivered_at, {{2014, 10, 28}, {20, 31, 42.4}}}]),
    Result = deliv_change:status(Change),
    ?assertEqual(delivered, Result).

status_returns_approved_if_approved() ->
    PipeId = <<"1">>,
    Change = deliv_change:fromlist([{approved_by, <<"Brian">>}, {pipeline_id, PipeId}]),

    hoax:mock(deliv_dependency_failures,
              ?expect(is_blocked,
                      ?withArgs([PipeId]),
                      ?andReturn(false))),

    Result = deliv_change:status(Change),
    ?assertEqual(approved, Result).

status_returns_open_if_open() ->
    PipeId = <<"1">>,
    Change = deliv_change:fromlist([{pipeline_id, PipeId}]),

    hoax:mock(deliv_dependency_failures,
              ?expect(is_blocked,
                      ?withArgs([PipeId]),
                      ?andReturn(false))),

    Result = deliv_change:status(Change),
    ?assertEqual(open, Result).

status_returns_superseded_if_superseded() ->
    Change = deliv_change:fromlist([{merge_sha, <<"foo">>}, {superseding_change_id, <<"abc-123">>}]),
    Result = deliv_change:status(Change),
    ?assertEqual(superseded, Result).

status_returns_blocked_if_blocked() ->
    PipeId = <<"1">>,
    Change = deliv_change:fromlist([{pipeline_id, PipeId}]),

    hoax:mock(deliv_dependency_failures,
              ?expect(is_blocked,
                      ?withArgs([PipeId]),
                      ?andReturn(true))),

    Result = deliv_change:status(Change),
    ?assertEqual(blocked, Result).

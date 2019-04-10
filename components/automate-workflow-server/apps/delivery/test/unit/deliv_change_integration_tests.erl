-module(deliv_change_integration_tests).

-include_lib("hoax/include/hoax.hrl").

-compile(export_all).

patchset_for_new_feature_branch_fixture_test_() ->
    hoax:parameterized_fixture(?MODULE, "patchset_for_new_feature_branch", setup, teardown).

patchset_for_existing_feature_branch_fixture_test_() ->
    hoax:parameterized_fixture(?MODULE, "patchset_for_existing_feature_branch", setup, teardown).

change_to_ejson_fixture_test_() ->
    hoax:parameterized_fixture(?MODULE, "change_to_ejson", setup, teardown).

superseded_change_fixture_test_() ->
    hoax:parameterized_fixture(?MODULE, "superseded_change", setup, teardown).

branch_of_branch_fixture_test_() ->
    hoax:parameterized_fixture(?MODULE, "branch_of_branch", setup, teardown).

setup() ->
    error_logger:tty(false),
    eu_database:setup(),
    setup_change_subscriptions(),
    eu_data:with_enterprise(<<"deliv_change_test_enterprise">>,
       eu_data:with_organization(<<"deliv_change_test_organization">>,
        eu_data:with_project(<<"deliv_change_test_project">>,
          eu_data:with_pipeline(<<"master">>,
            fun(Enterprise, Organization, Project, Pipeline) ->
                User = eu_data:fetch_or_create_user(Enterprise, <<"deliv_change_eunit_user">>),
                {Enterprise, User, Organization, Project, Pipeline}
            end)))).

teardown(_) ->
    eu_database:teardown(),
    application:stop(gproc),
    error_logger:tty(true).

setup_change_subscriptions() ->
    application:set_env(delivery, stages_data, [
                                                {verify, do_not_care},
                                                {build, do_not_care},
                                                {acceptance, do_not_care},
                                                {union, do_not_care},
                                                {rehearsal, do_not_care},
                                                {delivered, do_not_care}
                                               ]),
    application:start(gproc).

patchset_for_new_feature_branch_creates_change({Enterprise, User, Organization, Project, Pipeline}) ->
    FeatureBranch = <<"deliv_change_eunit_feature">>,
    ChangePatchset = eu_data:create_patchset(Enterprise, User, Organization, Project, Pipeline, FeatureBranch),
    Change = eu_data:change_from_patchset(ChangePatchset),

    ?assertEqual(deliv_patchset:getval(sequence_number, ChangePatchset),
                 deliv_change:getval(latest_patchset, Change)),
    ?assertEqual(deliv_pipeline:getval(name, Pipeline),
                 deliv_change:getval(pipeline_name_at_creation, Change)).

patchset_for_existing_feature_branch_updates_change({Enterprise, User, Organization, Project, Pipeline}) ->
    %% Arrange
    FeatureBranch = <<"deliv_change_eunit_feature">>,

    %% Act (create two patchset in a change)
    %% Creates change off of feature branch, as verified in other test
    FirstPatchset = eu_data:create_patchset(Enterprise, User, Organization, Project, Pipeline, FeatureBranch),
    ChangeAtFirstPatchset = eu_data:change_from_patchset(FirstPatchset),
    %% Updates change from existing feature branch
    SecondPatchset = eu_data:create_patchset(Enterprise, User, Organization, Project, Pipeline, FeatureBranch),
    ChangeAtSecondPatchset = eu_data:change_from_patchset(SecondPatchset),

    %% Assert
    ?assertEqual(deliv_change:getval(id, ChangeAtFirstPatchset),
                 deliv_change:getval(id, ChangeAtSecondPatchset)),
    ?assertNotEqual(deliv_patchset:getval(sequence_number, FirstPatchset),
                    deliv_patchset:getval(sequence_number, SecondPatchset)),
    ?assertEqual(deliv_patchset:getval(sequence_number, SecondPatchset),
                 deliv_change:getval(latest_patchset, ChangeAtSecondPatchset)).

change_to_ejson_when_record_new_change_should_format_to_ejson({Enterprise, User, Organization, Project, Pipeline}) ->
    %% Arrange
    FeatureBranch = <<"deliv_change_eunit_feature">>,

    %% Creates change off of feature branch, as verified in other test
    Patchset = eu_data:create_patchset(Enterprise, User, Organization, Project, Pipeline, FeatureBranch),
    Change = eu_data:change_from_patchset(Patchset),

    ChangeId = deliv_change:getval(id, Change),

    Fields = deliv_change_common:ejson_values_from_change(Change),
    ExpectedFields = [
                  {<<"id">>,           ChangeId},
                  {<<"topic">>,        FeatureBranch},
                  {<<"target">>,       deliv_change:getval(pipeline_name_at_creation, Change)},
                  {<<"state">>,        deliv_change:getval(latest_patchset_status, Change)},
                  {<<"submit_at">>,    chef_utils:format_timestamp((deliv_change:getval(submitted_at, Change)))},
                  {<<"submit_by">>,    deliv_change:getval(submitted_by, Change)},
                  {<<"merge_sha">>,    deliv_change:getval(merge_sha, Change)},
                  {<<"approved_by">>,  deliv_change:getval(approved_by, Change)},
                  {<<"title">>,        deliv_change:getval(title, Change)},
                  {<<"description">>,  deliv_change:getval(description, Change)},
                  {<<"delivered_by">>, deliv_change:getval(delivered_by, Change)},
                  {<<"delivered_at">>, <<"">>}
                 ],
    [?assertEqual(Val1, Val2) || {Val1, Val2} <- lists:zip(ExpectedFields, Fields)].

superseded_change_should_update_merged_but_not_accepted_changes_for_pipeline({Enterprise, User, Organization, Project, Pipeline}) ->
    FeatureBranch = <<"deliv_change_eunit_feature">>,

    ChangePatchset = eu_data:create_patchset(Enterprise, User, Organization, Project, Pipeline, FeatureBranch),
    Change = eu_data:change_from_patchset(ChangePatchset),
    ChangeId = deliv_change:getval(id, Change),

    ChangePatchset2 = eu_data:create_patchset(Enterprise, User, Organization, Project, Pipeline, <<"bar">>),
    SupersedingChange = eu_data:change_from_patchset(ChangePatchset2),
    SupersedingChangeId = deliv_change:getval(id, SupersedingChange),

    deliv_stage_run:insert([{change_id, ChangeId},
                            {stage, <<"verify">>},
                            {status, <<"passed">>},
                            {finished, true}]),
    deliv_stage_run:insert([{change_id, SupersedingChangeId},
                            {stage, <<"verify">>},
                            {status, <<"passed">>},
                            {finished, true}]),

    hoax:mock(deliv_stage,
        ?expect(trigger,
            ?withArgs([build, '_']),
            ?andReturn(ignored))),

    hoax:mock(deliv_scm_local,
        ?expect(merge_feature_branch,
            ?withArgs(['_', '_', '_']),
            ?andReturn({ok, <<"foo">>}))),

    hoax:mock(deliv_scm_local,
        ?expect(delete_feature_branch,
            ?withArgs(['_', '_']),
            ?andReturn({ok, ignored}))),

    hoax:mock(deliv_patchset_changed_file,
              ?expect(refresh_cache,
                      ?withArgs(['_', '_', '_']),
                      ?times(2))),

    hoax:mock(deliv_event, [
              ?expect(publish,
                      ?withArgs([{change_approved, ChangeId}, ?any])),
              ?expect(publish,
                      ?withArgs([{change_approved, SupersedingChangeId}, ?any])),
              ?expect(publish,
                      ?withArgs([change_approved, ?any]),
                      ?times(2)),
              ?expect(publish,
                      ?withArgs([{change_superseded, ChangeId}, ?any])),
              ?expect(publish,
                      ?withArgs([change_superseded, ?any]),
                      ?times(1))
    ]),

    deliv_change:merge(Change, User),
    deliv_changeset:add_to_changeset(Change, []),

    deliv_change:merge(SupersedingChange, User),
    deliv_changeset:add_to_changeset(SupersedingChange, []),

    {ok, SupersededChange} = deliv_change:fetch_by_id(ChangeId),
    {ok, UpdatedSupersedingChange} = deliv_change:fetch_by_id(SupersedingChangeId),

    ?assertEqual(true, deliv_change:is_merged(SupersededChange)),
    ?assertEqual({ok, UpdatedSupersedingChange}, deliv_change:get_superseding_change(SupersededChange)),
    ?verifyAll.

branch_of_branch_should_recalculate_changeset_on_merge({Enterprise, User, Organization,
                                                        Project, Pipeline}) ->
    FeatureBranch = <<"deliv_eunit_branch">>,
    Patchset = eu_data:create_patchset(Enterprise, User, Organization, Project,
                                       Pipeline, FeatureBranch),
    Change = eu_data:change_from_patchset(Patchset),
    ChangeId = deliv_change:getval(id, Change),
    File = eu_data:create_patchset_changed_file(Patchset, <<"added">>, <<"a">>, 1, 0),
    File2 = eu_data:create_patchset_changed_file(Patchset, <<"added">>, <<"b">>, 1, 0),

    FeatureBranch2 = <<"deliv_eunit_subbranch">>,
    Patchset2 = eu_data:create_patchset(Enterprise, User, Organization, Project,
                                        Pipeline, FeatureBranch2),
    PatchsetId2 = deliv_patchset:getval(id, Patchset2),
    Change2 = eu_data:change_from_patchset(Patchset2),
    ChangeId2 = deliv_change:getval(id, Change2),
    eu_data:create_patchset_changed_file(Patchset2, <<"added">>, <<"a">>, 1, 0),
    eu_data:create_patchset_changed_file(Patchset2, <<"added">>, <<"b">>, 1, 0),
    File3 = eu_data:create_patchset_changed_file(Patchset2, <<"added">>, <<"c">>, 1, 0),

    deliv_stage_run:insert([{change_id, ChangeId},
                            {stage, <<"verify">>},
                            {status, <<"passed">>},
                            {finished, true}]),
    deliv_stage_run:insert([{change_id, ChangeId2},
                            {stage, <<"verify">>},
                            {status, <<"passed">>},
                            {finished, true}]),

    % Git/stage operations are stubbed, but all the database operations are live
    hoax:mock(deliv_stage,
        ?expect(trigger,
            ?withArgs([build, '_']),
            ?andReturn(ignored))),

    hoax:mock(deliv_event, [
              ?expect(publish,
                      ?withArgs([{change_approved, ChangeId}, ?any])),
              ?expect(publish,
                      ?withArgs([{change_approved, ChangeId2}, ?any])),
              ?expect(publish,
                      ?withArgs([change_approved, ?any]),
                      ?times(2)),
              ?expect(publish,
                      ?withArgs([{change_superseded, ChangeId}, ?any]),
                      ?times(1)),
              ?expect(publish,
                      ?withArgs([change_superseded, ?any]),
                      ?times(1))
    ]),

    hoax:mock(deliv_scm_local,
        ?expect(merge_feature_branch,
            ?withArgs(['_', '_', '_']),
            ?andReturn({ok, <<"foo">>}))),

    hoax:mock(deliv_scm_local,
        ?expect(delete_feature_branch,
            ?withArgs(['_', '_']),
            ?andReturn({ok, ignored}))),

    hoax:mock(deliv_git,
              ?expect(patchset_changed_files,
                      ?withArgs(['_', '_', '_', '_', '_', FeatureBranch, '_']),
                      ?andReturn({ok, [File, File2]}))),

    hoax:mock(deliv_git,
              ?expect(patchset_changed_files,
                      ?withArgs(['_', '_', '_', '_', '_', FeatureBranch2, '_']),
                      ?andReturn({ok, [File3]}))),

    % Merge first branch:
    deliv_change:merge(ChangeId, User),
    deliv_changeset:add_to_changeset(Change, []),

    % DB should not yet be cleaned up for unmerged changes:
    {ok, Result2} = deliv_db:fetch2(deliv_patchset_changed_file, patchset_id, PatchsetId2),
    ?assertEqual([{deliv_patchset_changed_file, PatchsetId2, <<"added">>, <<"a">>,
                   <<"1">>, <<"0">>},
                  {deliv_patchset_changed_file, PatchsetId2, <<"added">>, <<"b">>,
                   <<"1">>, <<"0">>},
                  {deliv_patchset_changed_file, PatchsetId2, <<"added">>, <<"c">>,
                   <<"1">>, <<"0">>}],
                 Result2),

    % Merge sub-branch; verify DB is cleaned up correctly:
    deliv_change:merge(ChangeId2, User),
    deliv_changeset:add_to_changeset(Change2, []),
    {ok, Result3} = deliv_db:fetch2(deliv_patchset_changed_file, patchset_id, PatchsetId2),
    ?assertEqual([{deliv_patchset_changed_file, PatchsetId2, <<"added">>, <<"c">>,
                   <<"1">>, <<"0">>}],
                 Result3),
    ?verifyAll.

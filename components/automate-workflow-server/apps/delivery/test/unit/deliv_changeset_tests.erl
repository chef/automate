%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
-module(deliv_changeset_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("deliv_types.hrl").

-compile(export_all).

add_to_changeset_fixture_test_() ->
    hoax:parameterized_fixture(?MODULE, "add_to_changeset", setup_with_one_change, teardown).

with_two_changes_add_to_changeset_fixture_test_() ->
    hoax:parameterized_fixture(?MODULE, "with_two_changes", setup_with_two_changes, teardown).

with_three_changes_add_to_changeset_fixture_test_() ->
    hoax:parameterized_fixture(?MODULE, "with_three_changes", setup_with_three_changes, teardown).

dependency_ejson_for_project_fixture_test_() ->
    hoax:fixture(?MODULE, "dependency_ejson_for_project", basic_setup, teardown).

dependency_ejson_for_project_parameterized_fixture_test_() ->
    hoax:parameterized_fixture(?MODULE, "dependency_ejson_for_project", setup_dependencies, teardown).

consumer_change_ids_for_pipeline_parameterized_fixture_test_() ->
    hoax:parameterized_fixture(?MODULE, "consumer_change_ids_for_pipeline", setup_dependencies, teardown).

latest_closed_for_pipeline_fixture_test_() ->
    hoax:parameterized_fixture(?MODULE, "latest_closed_for_pipeline", setup_with_closed, teardown).

setup(ChangeCreationFun) ->
    basic_setup(),
    eu_data:with_enterprise(<<"deliv_changeset_test_enterprise">>,
       eu_data:with_organization(<<"deliv_changeset_test_organization">>,
        eu_data:with_project(<<"deliv_changeset_test_project">>,
          eu_data:with_pipeline(<<"master">>,ChangeCreationFun)))).

basic_setup() ->
    error_logger:tty(false),
    application:start(gproc),
    eu_database:setup().

setup_with_one_change() ->
    setup(
    fun(Enterprise, Organization, Project, Pipeline) ->
        User = eu_data:fetch_or_create_user(Enterprise, <<"deliv_changeset_eunit_user">>),

        FeatureBranch = <<"deliv_changeset_eunit_feature">>,
        Change1Patchset = eu_data:create_patchset(Enterprise, User, Organization, Project, Pipeline, FeatureBranch),
        Change1 = eu_data:change_from_patchset(Change1Patchset),

        Change1
      end).

setup_with_two_changes() ->
    setup(fun(Enterprise, Organization, Project, Pipeline) ->
        User = eu_data:fetch_or_create_user(Enterprise, <<"deliv_changeset_eunit_user">>),

        FeatureBranch = <<"deliv_changeset_eunit_feature">>,
        Change1Patchset = eu_data:create_patchset(Enterprise, User, Organization, Project, Pipeline, FeatureBranch),
        Change1 = eu_data:change_from_patchset(Change1Patchset),

        FeatureBranch2 = <<"deliv_changeset_eunit_feature2">>,
        Change2Patchset = eu_data:create_patchset(Enterprise, User, Organization, Project, Pipeline, FeatureBranch2),
        Change2 = eu_data:change_from_patchset(Change2Patchset),

        [Change1, Change2]
    end).

setup_with_three_changes() ->
    setup(fun(Enterprise, Organization, Project, Pipeline) ->
        User = eu_data:fetch_or_create_user(Enterprise, <<"deliv_changeset_eunit_user">>),

        FeatureBranch = <<"deliv_changeset_eunit_feature">>,
        Change1Patchset = eu_data:create_patchset(Enterprise, User, Organization, Project, Pipeline, FeatureBranch),
        Change1 = eu_data:change_from_patchset(Change1Patchset),

        FeatureBranch2 = <<"deliv_changeset_eunit_feature2">>,
        Change2Patchset = eu_data:create_patchset(Enterprise, User, Organization, Project, Pipeline, FeatureBranch2),
        Change2 = eu_data:change_from_patchset(Change2Patchset),

        FeatureBranch3 = <<"deliv_changeset_eunit_feature3">>,
        Change3Patchset = eu_data:create_patchset(Enterprise, User, Organization, Project, Pipeline, FeatureBranch3),
        Change3 = eu_data:change_from_patchset(Change3Patchset),

        [Change1, Change2, Change3]
    end).

setup_with_closed() ->
    setup(fun(Enterprise, Organization, Project, Pipeline) ->
        User = eu_data:fetch_or_create_user(Enterprise, <<"deliv_changeset_eunit_user">>),

        FeatureBranch = <<"deliv_changeset_eunit_feature1">>,
        Change1Patchset = eu_data:create_patchset(Enterprise, User, Organization, Project, Pipeline, FeatureBranch),
        Change1 = eu_data:change_with_closed_changeset_from_patchset(Change1Patchset, User, []),

        Change2Patchset = eu_data:create_patchset(Enterprise, User, Organization, Project, Pipeline, FeatureBranch),
        Change2 = eu_data:change_with_closed_changeset_from_patchset(Change2Patchset, User, []),

        Change3Patchset = eu_data:create_patchset(Enterprise, User, Organization, Project, Pipeline, FeatureBranch),
        Change3 = eu_data:change_with_closed_changeset_from_patchset(Change3Patchset, User, []),

        [Change1, Change2, Change3]
    end).

setup_dependencies() ->
    setup(fun(Enterprise, Organization, Project, Pipeline) ->
        Project2 = eu_data:fetch_or_create_project(Enterprise, Organization, <<"deliv_changeset_test_project2">>),
        Project3 = eu_data:fetch_or_create_project(Enterprise, Organization, <<"deliv_changeset_test_project3">>),
        Project4 = eu_data:fetch_or_create_project(Enterprise, Organization, <<"deliv_changeset_test_project4">>),
        Project5 = eu_data:fetch_or_create_project(Enterprise, Organization, <<"deliv_changeset_test_project5">>),

        Pipeline2 = eu_data:fetch_or_create_pipeline(Enterprise, Organization, Project2, <<"master">>),
        Pipeline3 = eu_data:fetch_or_create_pipeline(Enterprise, Organization, Project3, <<"master">>),
        Pipeline4 = eu_data:fetch_or_create_pipeline(Enterprise, Organization, Project4, <<"master">>),
        Pipeline5 = eu_data:fetch_or_create_pipeline(Enterprise, Organization, Project5, <<"master">>),
        Pipeline6 = eu_data:fetch_or_create_pipeline(Enterprise, Organization, Project, <<"not-master">>),

        PipelineId = deliv_pipeline:getval(id, Pipeline),
        Pipeline2Id = deliv_pipeline:getval(id, Pipeline2),
        Pipeline3Id = deliv_pipeline:getval(id, Pipeline3),

        User = eu_data:fetch_or_create_user(Enterprise, <<"deliv_changeset_eunit_user">>),

        %% create and close changeset on the main project
        FeatureBranch = <<"deliv_changeset_eunit_feature">>,
        Change1Patchset = eu_data:create_patchset(Enterprise, User, Organization, Project, Pipeline, FeatureBranch),
        eu_data:change_with_closed_changeset_from_patchset(Change1Patchset, User, [Pipeline2Id, Pipeline3Id]),

        %% create a second, open changeset on the main project
        Change2Patchset = eu_data:create_patchset(Enterprise, User, Organization, Project, Pipeline, FeatureBranch),
        Change2 = eu_data:change_from_patchset(Change2Patchset),
        deliv_changeset:add_to_changeset(Change2, []),

        %% create and close a changeset on a consumer that will depend on the main project. move this changeset to union.
        FeatureBranch2 = <<"deliv_changeset_eunit_feature2">>,
        Change3Patchset = eu_data:create_patchset(Enterprise, User, Organization, Project4, Pipeline4, FeatureBranch2),
        eu_data:change_with_closed_changeset_from_patchset(Change3Patchset, User, [PipelineId]),

        %% create and close a second changeset in our consumer to ensure we only return PipelineId once
        Change4Patchset = eu_data:create_patchset(Enterprise, User, Organization, Project4, Pipeline4, FeatureBranch2),
        eu_data:change_with_closed_changeset_from_patchset(Change4Patchset, User, [PipelineId, Pipeline3Id]),

        %% create an open changeset for a potential consumer to ensure we only pull from current consumers (those in union)
        FeatureBranch3 = <<"deliv_changeset_eunit_feature3">>,
        Change5Patchset = eu_data:create_patchset(Enterprise, User, Organization, Project5, Pipeline5, FeatureBranch3),
        eu_data:change_with_closed_changeset_from_patchset(Change5Patchset, User, [PipelineId]),

        %% remove dep from changeset such that it wont show up in required_by
        Change6Patchset = eu_data:create_patchset(Enterprise, User, Organization, Project5, Pipeline5, FeatureBranch3),
        eu_data:change_with_closed_changeset_from_patchset(Change6Patchset, User, []),

        %% add the dependency back again. shouldnt show up in list since it hasn't been closed
        Change7Patchset = eu_data:create_patchset(Enterprise, User, Organization, Project5, Pipeline5, FeatureBranch3),
        Change7 = eu_data:change_from_patchset(Change7Patchset),
        deliv_changeset:add_to_changeset(Change7, [PipelineId]),

        %% add a dependency to the not-master pipeline of project
        Change8Patchset = eu_data:create_patchset(Enterprise, User, Organization, Project, Pipeline6, FeatureBranch2),
        eu_data:change_with_closed_changeset_from_patchset(Change8Patchset, User, [Pipeline3Id]),

        []
    end).

teardown(_) ->
    eu_database:teardown(),
    application:stop(gproc),
    error_logger:tty(true).

add_to_changeset_fails_with_a_nonexistent_change(_) ->
    ChangeId = <<"aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee">>,
    Change = deliv_change:fromlist([{id, ChangeId}]),
    DepIds = [1,2,3],

    ?assertEqual({error, {<<"CD018">>, <<"Unknown change">>}},
                 deliv_changeset:add_to_changeset(Change, DepIds)).

add_to_changeset_returns_an_open_changeset_when_given_an_existing_change(Change) ->
    DepIds = [1,2,3],
    {ok, Changeset} = deliv_changeset:add_to_changeset(Change, DepIds),
    assert_change_in_changeset(Change, Changeset),
    assert_deps_in_changeset(DepIds, Changeset),
    ?assertEqual(<<"open">>,
                 deliv_changeset:getval(status, Changeset)).

add_to_changeset_updates_latest_change_id(Change) ->
    {ok, Changeset} = deliv_changeset:add_to_changeset(Change, []),
    assert_latest_change_id_in_changeset(Changeset, Change).

assert_latest_change_id_in_changeset(Changeset, Change) ->
    ChangeId = deliv_change:getval(id, Change),
    LatestChangeId = deliv_changeset:getval(latest_change_id, Changeset),
    ?assertEqual(ChangeId, LatestChangeId).

with_two_changes_add_to_changeset_updates_dependencies_when_changeset_exists([Change1, Change2]) ->
    DepIds1 = [1,2,3],
    DepIds2 = [2,3,4],
    {ok, Changeset} = deliv_changeset:add_to_changeset(Change1, DepIds1),
    {ok, Changeset2} = deliv_changeset:add_to_changeset(Change2, DepIds2),

    ?assertEqual(deliv_changeset:getval(id, Changeset),
                 deliv_changeset:getval(id, Changeset2)),
    assert_deps_in_changeset(DepIds2, Changeset2).

with_two_changes_add_to_changeset_adds_to_an_existing_changeset_when_one_exists([Change1, Change2]) ->
    DepIds = [1,2,3],
    {ok, Changeset} = deliv_changeset:add_to_changeset(Change1, DepIds),
    {ok, Changeset2} = deliv_changeset:add_to_changeset(Change2, DepIds),

    ?assertEqual(deliv_changeset:getval(id, Changeset),
                 deliv_changeset:getval(id, Changeset2)),
    assert_deps_in_changeset(DepIds, Changeset).

with_two_changes_closing_a_changeset_records_deliverer_information_on_each_change([Change1, Change2]) ->
    DeliveringUser = eu_data:with_enterprise(<<"deliv_changeset_test_enterprise">>,
                                             fun(Enterprise) ->
                                               eu_data:fetch_or_create_user(Enterprise, <<"delivering_user">>)
                                             end),
    {ok, _Changeset} = deliv_changeset:add_to_changeset(Change1, []),
    {ok, Changeset1} = deliv_changeset:add_to_changeset(Change2, []),
    Change1Id = deliv_change:getval(id, Change1),
    Change2Id = deliv_change:getval(id, Change2),

    hoax:mock(deliv_event,
                ?expect(publish,
                    ?withArgs([change_delivered, ?any]))),

    ok = deliv_changeset:close_changeset(Change1Id, DeliveringUser),

    [[change_delivered, PublishedChange2],
     [change_delivered, PublishedChange1]] = hoax:arguments(fun deliv_event:publish/2),

    PublishedChangeId1 = deliv_change:getval(id, PublishedChange1),
    PublishedChangeId2 = deliv_change:getval(id, PublishedChange2),

    ?assertEqual(Change1Id, PublishedChangeId1),
    ?assertEqual(Change2Id, PublishedChangeId2),

    ?verifyAll,
    assert_change_and_changeset_share_delivery_metadata(DeliveringUser, Changeset1, [Change1, Change2]).

with_two_changes_where_one_failed_a_build_add_to_changeset_should_update_changeset_id_on_failed_change([ChangeOne, ChangeTwo]) ->
  ChangeOneId = deliv_change:getval(id, ChangeOne),
  ChangeTwoId = deliv_change:getval(id, ChangeTwo),
  deliv_stage_run:insert([{change_id, ChangeOneId},
                          {stage, <<"verify">>},
                          {status, <<"passed">>}]),
  deliv_stage_run:insert([{change_id, ChangeOneId},
                          {stage, <<"build">>},
                          {status, <<"failed">>}]),
  deliv_stage_run:insert([{change_id, ChangeTwoId},
                          {stage, <<"verify">>},
                          {status, <<"passed">>}]),
  deliv_stage_run:insert([{change_id, ChangeTwoId},
                          {stage, <<"build">>},
                          {status, <<"passed">>}]),
  deliv_stage_run:insert([{change_id, ChangeTwoId},
                          {stage, <<"acceptance">>},
                          {status, <<"passed">>}]),

  {ok, Change} = deliv_change:fetch_by_id(ChangeOneId),
  UpdatedChange = deliv_change:setvals([{merge_sha, <<"foo">>}], Change),
  deliv_change:update(UpdatedChange),
  deliv_changeset:add_to_changeset(ChangeTwo, []),

  {ok, UpdatedChangeOne} = deliv_change:fetch_by_id(ChangeOneId),
  {ok, UpdatedChangeTwo} = deliv_change:fetch_by_id(ChangeTwoId),

  ChangeOneChangesetId = deliv_change:getval(changeset_id, UpdatedChangeOne),
  ChangeTwoChangesetId = deliv_change:getval(changeset_id, UpdatedChangeTwo),

  ?assertEqual(ChangeOneChangesetId, ChangeTwoChangesetId).

with_two_changes_where_one_is_still_running_in_build_add_to_changeset_should_not_set_building_changes_superseding_change_id([ChangeOne, ChangeTwo]) ->
  ChangeOneId = deliv_change:getval(id, ChangeOne),
  ChangeTwoId = deliv_change:getval(id, ChangeTwo),
  deliv_stage_run:insert([{change_id, ChangeOneId},
                          {stage, <<"verify">>},
                          {status, <<"passed">>}]),
  deliv_stage_run:insert([{change_id, ChangeOneId},
                          {stage, <<"build">>},
                          {status, <<"running">>}]),
  deliv_stage_run:insert([{change_id, ChangeTwoId},
                          {stage, <<"verify">>},
                          {status, <<"passed">>}]),
  deliv_stage_run:insert([{change_id, ChangeTwoId},
                          {stage, <<"build">>},
                          {status, <<"passed">>}]),
  deliv_stage_run:insert([{change_id, ChangeTwoId},
                          {stage, <<"acceptance">>},
                          {status, <<"passed">>}]),

  UpdatedChangeOne = deliv_change:setvals([{merge_sha, <<"foo">>}, {approved_at, {{2016,2,10},{0,32,47}}}], ChangeOne),
  deliv_change:update(UpdatedChangeOne),
  UpdatedChangeTwo = deliv_change:setvals([{merge_sha, <<"bar">>}, {approved_at, {{2016,2,10},{22,55,6}}}], ChangeOne),
  deliv_change:update(UpdatedChangeTwo),
  deliv_changeset:add_to_changeset(UpdatedChangeOne, []),

  ChangeTwoSupersededChangesetId = deliv_change:getval(changeset_id, UpdatedChangeTwo),

  ?assertEqual(undefined, ChangeTwoSupersededChangesetId).

%% This test path is not one that a user should be able to hit through the UI.
%% However, it replicates the repro steps of a bug that has been seen both
%% locally and in customers.
with_two_changes_where_both_failed_acceptance_and_the_superseded_change_is_rerun_the_superseding_change_has_no_superseded_change([ChangeOne, ChangeTwo]) ->
    ChangeOneId = deliv_change:getval(id, ChangeOne),
    ChangeTwoId = deliv_change:getval(id, ChangeTwo),
    deliv_stage_run:insert([{change_id, ChangeOneId},
                            {stage, <<"verify">>},
                            {status, <<"passed">>}]),
    deliv_stage_run:insert([{change_id, ChangeOneId},
                            {stage, <<"build">>},
                            {status, <<"passed">>}]),
    deliv_stage_run:insert([{change_id, ChangeOneId},
                            {stage, <<"acceptance">>},
                            {status, <<"failed">>}]),
    deliv_stage_run:insert([{change_id, ChangeTwoId},
                            {stage, <<"verify">>},
                            {status, <<"passed">>}]),
    deliv_stage_run:insert([{change_id, ChangeTwoId},
                            {stage, <<"build">>},
                            {status, <<"passed">>}]),
    deliv_stage_run:insert([{change_id, ChangeTwoId},
                            {stage, <<"acceptance">>},
                            {status, <<"failed">>}]),

    {ok, Change1} = deliv_change:fetch_by_id(ChangeOneId),
    UpdatedChange1 = deliv_change:setvals([{merge_sha, <<"foo">>}], Change1),
    deliv_change:update(UpdatedChange1),
    deliv_changeset:add_to_changeset(UpdatedChange1, []),
    {ok, Change2} = deliv_change:fetch_by_id(ChangeTwoId),
    UpdatedChange2 = deliv_change:setvals([{merge_sha, <<"bar">>}], Change2),
    deliv_change:update(UpdatedChange2),
    deliv_changeset:add_to_changeset(UpdatedChange2, []),

    deliv_stage_run:insert([{change_id, ChangeOneId},
                            {stage, <<"acceptance">>},
                            {status, <<"failed">>}]),
    deliv_changeset:add_to_changeset(UpdatedChange1, []),

    {ok, SupersededChange} = deliv_change:fetch_by_id(ChangeTwoId),
    {ok, SupersedingChange} = deliv_change:fetch_by_id(ChangeOneId),

    ?assertEqual({ok, SupersedingChange}, deliv_change:get_superseding_change(SupersededChange)),
    ?assertEqual(undefined, deliv_change:get_superseding_change(SupersedingChange)).

with_three_changes_where_verify_passed_and_build_failed_add_to_changeset_should_not_update_changeset_id_on_verify_changes([Change1, Change2, Change3]) ->
  ChangeId1 = deliv_change:getval(id, Change1),
  ChangeId2 = deliv_change:getval(id, Change2),
  ChangeId3 = deliv_change:getval(id, Change3),
  deliv_stage_run:insert([{change_id, ChangeId1},
                          {stage, <<"verify">>},
                          {finished, true},
                          {status, <<"failed">>}]),
  deliv_stage_run:insert([{change_id, ChangeId2},
                          {stage, <<"verify">>},
                          {finished, true},
                          {status, <<"passed">>}]),
  deliv_stage_run:insert([{change_id, ChangeId2},
                          {stage, <<"build">>},
                          {finished, true},
                          {status, <<"failed">>}]),
  deliv_stage_run:insert([{change_id, ChangeId3},
                          {stage, <<"verify">>},
                          {finished, true},
                          {status, <<"passed">>}]),
  deliv_stage_run:insert([{change_id, ChangeId3},
                          {stage, <<"build">>},
                          {finished, true},
                          {status, <<"passed">>}]),
  deliv_stage_run:insert([{change_id, ChangeId3},
                          {stage, <<"acceptance">>},
                          {finished, true},
                          {status, <<"passed">>}]),

  MergedChange2 = deliv_change:setvals([{merge_sha, <<"foo">>}], Change2),
  deliv_change:update(MergedChange2),
  deliv_changeset:add_to_changeset(Change3, []),

  {ok, UpdatedChange1} = deliv_change:fetch_by_id(ChangeId1),
  {ok, UpdatedChange2} = deliv_change:fetch_by_id(ChangeId2),
  {ok, UpdatedChange3} = deliv_change:fetch_by_id(ChangeId3),
  Change1ChangesetId = deliv_change:getval(changeset_id, UpdatedChange1),
  Change2ChangesetId = deliv_change:getval(changeset_id, UpdatedChange2),
  Change3ChangesetId = deliv_change:getval(changeset_id, UpdatedChange3),
  ?assertEqual(Change2ChangesetId, Change3ChangesetId),
  ?assertEqual(undefined, Change1ChangesetId),
  ?assertNotEqual(undefined, Change2ChangesetId),
  ?assertNotEqual(undefined, Change3ChangesetId).

assert_change_and_changeset_share_delivery_metadata(DeliveringUser, Changeset, Changes) ->
    UpdatedChanges = [ refresh_change(C) || C <- Changes ],
    UpdatedChangeset = refresh_changeset(Changeset),

    DeliveredAt = deliv_changeset:getval(delivered_at, UpdatedChangeset),
    DeliveredBy = deliv_changeset:getval(delivered_by, UpdatedChangeset),

    ?assertEqual(DeliveredBy, deliv_user:getval(name, DeliveringUser)),

    [ ?assertEqual(DeliveredAt, deliv_change:getval(delivered_at, C)) || C <- UpdatedChanges ],
    [ ?assertEqual(DeliveredBy, deliv_change:getval(delivered_by, C)) || C <- UpdatedChanges ].

refresh_changeset(Changeset) ->
    ChangesetId = deliv_changeset:getval(id, Changeset),
    {ok, UpdatedChangeset} = deliv_changeset:fetch_by_id(ChangesetId),
    UpdatedChangeset.

refresh_change(Change) ->
    ChangeId = deliv_change:getval(id, Change),
    {ok, UpdatedChange} = deliv_change:fetch_by_id(ChangeId),
    UpdatedChange.

assert_change_in_own_changeset(Change) ->
    UpdatedChange = refresh_change(Change),
    ChangesetId = deliv_change:getval(changeset_id, UpdatedChange),

    ?assertNotEqual(undefined, ChangesetId),

    {ok, Changeset} = deliv_changeset:fetch_by_id(ChangesetId),
    assert_change_in_changeset(UpdatedChange, Changeset).

assert_change_in_changeset(Change, Changeset) ->
    UpdatedChange = refresh_change(Change),

    ChangesetId = deliv_changeset:getval(id, Changeset),

    %% Make sure the IDs match
    ?assertEqual(deliv_change:getval(changeset_id, UpdatedChange),
                 ChangesetId),

    %% Also verify the change shows up in the "changelog"
    Changelog = deliv_change:changelog_for_changeset(ChangesetId),
    ?assert(lists:member(UpdatedChange, Changelog)).

assert_deps_in_changeset(DepIds, Changeset) ->
    ChangesetDepIds = deliv_changeset:getval(dependencies, Changeset),
    ?assertEqual(DepIds, ChangesetDepIds).

dependency_ejson_for_project_errors_for_bad_dep_call(_) ->
    EntName = <<"deliv_changeset_test_enterprise">>,
    OrgName = <<"deliv_changeset_test_organization">>,
    ProjName = <<"deliv_changeset_test_project">>,

    hoax:mock(deliv_db,
              ?expect(select,
                      ?withArgs([deliv_changeset, dependencies_by_project, [EntName, OrgName, ProjName],
                                 rows_as_records, [dependency_summary, record_info(fields, dependency_summary)]]),
                      ?andReturn({error, db_error}))),

    Actual = deliv_changeset:dependency_ejson_for_project(EntName, OrgName, ProjName),
    ?assertEqual({error, {system_error, db_error}}, Actual),
    ?verifyAll.

dependency_ejson_for_project_errors_for_bad_scoping_names_call(_) ->
    EntName = <<"deliv_changeset_test_enterprise">>,
    OrgName = <<"deliv_changeset_test_organization">>,
    ProjName = <<"deliv_changeset_test_project">>,
    PipelineIds = [1,2,3],
    Pipeline = deliv_pipeline:'#new'(),

    hoax:mock(deliv_db,
              ?expect(select,
                      ?withArgs([deliv_changeset, dependencies_by_project, [EntName, OrgName, ProjName],
                                 rows_as_records, [dependency_summary, record_info(fields, dependency_summary)]]),
                      ?andReturn({ok, [#dependency_summary{pipeline_id = 1,
                                                           dependencies = PipelineIds,
                                                           consumers = PipelineIds}]}))),
    hoax:mock(deliv_pipeline, [
                          ?expect(fetch_by_id,
                                  ?withArgs([1]),
                                  ?andReturn({ok, Pipeline})),
                          ?expect(getval,
                                  ?withArgs([id, Pipeline]),
                                  ?andReturn(1)),
                          ?expect(getval,
                                  ?withArgs([name, Pipeline]),
                                  ?andReturn(<<"master">>)),
                          ?expect(scoping_names,
                                  ?withArgs([1]),
                                  ?andReturn({error, db_error}))
                        ]),

    Actual = deliv_changeset:dependency_ejson_for_project(EntName, OrgName, ProjName),
    ?assertEqual({error, db_error}, Actual),
    ?verifyAll.

dependency_ejson_for_project_returns_project_not_found_error_if_project_not_found() ->
  eu_data:with_enterprise(<<"deliv_change_test_enterprise">>,
     eu_data:with_organization(<<"deliv_change_test_organization">>,
          fun(_Enterprise, _Organization) ->
                Result = deliv_changeset:dependency_ejson_for_project(<<"deliv_change_test_enterprise">>,<<"deliv_change_test_organization">>,<<"deliv_change_test_project">>),
                ?assertEqual({error,{not_found, project}}, Result)
          end)).

dependency_ejson_for_project_returns_empty_list_when_no_pipelines() ->
    eu_data:with_enterprise(<<"deliv_change_test_enterprise">>,
       eu_data:with_organization(<<"deliv_change_test_organization">>,
        eu_data:with_project(<<"deliv_change_test_project">>,
            fun(_Enterprise, _Organization, _Project) ->
                Result = deliv_changeset:dependency_ejson_for_project(<<"deliv_change_test_enterprise">>,<<"deliv_change_test_organization">>,<<"deliv_change_test_project">>),
                Expected = {
                  [
                    {<<"dependencies">>, {[]}},
                    {<<"required_by">>, {[]}}
                  ]
                },
                ?assertEqual({ok, Expected}, Result)
            end))).

dependency_ejson_for_project_returns_empty_list_when_no_deps() ->
    eu_data:with_enterprise(<<"deliv_change_test_enterprise">>,
      eu_data:with_organization(<<"deliv_change_test_organization">>,
        eu_data:with_project(<<"deliv_change_test_project">>,
          eu_data:with_pipeline(<<"master">>,
              fun(Enterprise, Organization, Project, Pipeline) ->
                  User = eu_data:fetch_or_create_user(Enterprise, <<"deliv_change_eunit_user">>),
                  Patchset = eu_data:create_patchset(Enterprise, User, Organization, Project, Pipeline, "master"),
                  Change = eu_data:change_from_patchset(Patchset),
                  {ok, Changeset} = deliv_changeset:add_to_changeset(Change, []),
                  {ok, _} = deliv_db:update(Changeset),
                  Result = deliv_changeset:dependency_ejson_for_project(<<"deliv_change_test_enterprise">>,<<"deliv_change_test_organization">>,<<"deliv_change_test_project">>),

                  Expected = {
                    [
                      {<<"dependencies">>, {[]}},
                      {<<"required_by">>, {[]}}
                    ]
                  },

                  ?assertEqual({ok, Expected}, Result)
              end)))).

dependency_ejson_for_project_returns_valid_json(_) ->
    EntName = <<"deliv_changeset_test_enterprise">>,
    OrgName = <<"deliv_changeset_test_organization">>,
    ProjName = <<"deliv_changeset_test_project">>,
    DepProjName2 = <<"deliv_changeset_test_project2">>,
    DepProjName3 = <<"deliv_changeset_test_project3">>,
    DepProjName4 = <<"deliv_changeset_test_project4">>,

    Expected = {
      [
        {<<"dependencies">>, {[
            {<<"not-master">>, [
                {[
                    {<<"enterprise">>, EntName},
                    {<<"organization">>, OrgName},
                    {<<"project">>, DepProjName3},
                    {<<"pipeline">>, <<"master">>}
                ]}
            ]},
            {<<"master">>, [
                {[
                    {<<"enterprise">>, EntName},
                    {<<"organization">>, OrgName},
                    {<<"project">>, DepProjName3},
                    {<<"pipeline">>, <<"master">>}
                ]},
                {[
                    {<<"enterprise">>, EntName},
                    {<<"organization">>, OrgName},
                    {<<"project">>, DepProjName2},
                    {<<"pipeline">>, <<"master">>}
                ]}
            ]}
        ]}},
        {<<"required_by">>, {[
            {<<"not-master">>, []},
            {<<"master">>, [
              {[
                  {<<"enterprise">>, EntName},
                  {<<"organization">>, OrgName},
                  {<<"project">>, DepProjName4},
                  {<<"pipeline">>, <<"master">>}
              ]}
            ]}
          ]}
        }
      ]
    },
    Actual = deliv_changeset:dependency_ejson_for_project(EntName, OrgName, ProjName),
    ?assertEqual({ok, Expected}, Actual).

consumer_change_ids_for_pipeline_returns_consumer_change_ids(_) ->
    Enterprise = eu_data:fetch_or_create_enterprise(<<"deliv_changeset_test_enterprise">>),
    Organization = eu_data:fetch_or_create_organization(Enterprise, <<"deliv_changeset_test_organization">>),
    ProjName = <<"deliv_changeset_test_project">>,
    User = eu_data:fetch_or_create_user(Enterprise, <<"deliv_changeset_eunit_user">>),

    %% Get the pipeline id
    Project = eu_data:fetch_or_create_project(Enterprise, Organization, ProjName),
    Pipeline = eu_data:fetch_or_create_pipeline(Enterprise, Organization, Project, <<"master">>),
    PipelineId = deliv_pipeline:getval(id, Pipeline),

    %% Create a change on the consumer and get the consumer change id
    ConsumerProjName = <<"deliv_changeset_test_project4">>,
    ConsumerFeatureBranch = <<"deliv_changeset_test_feature">>,
    ConsumerProject = eu_data:fetch_or_create_project(Enterprise, Organization, ConsumerProjName),
    ConsumerPipeline = eu_data:fetch_or_create_pipeline(Enterprise, Organization, ConsumerProject, <<"master">>),
    ConsumerPatchset = eu_data:create_patchset(Enterprise, User, Organization, ConsumerProject, ConsumerPipeline, ConsumerFeatureBranch),
    ConsumerChange = eu_data:change_from_patchset(ConsumerPatchset),
    ConsumerChangeId = deliv_change:getval(id, ConsumerChange),
    deliv_changeset:add_to_changeset(ConsumerChange, [PipelineId]),
    deliv_changeset:close_changeset(ConsumerChangeId, User),

    Expected = [ConsumerChangeId],
    Actual = deliv_changeset:consumer_change_ids_for_pipeline(PipelineId),
    ?assertEqual({ok, Expected}, Actual),
    ?verifyAll.

latest_closed_for_pipeline_returns_latest_changeset([Change1, _Change2, Change3]) ->
    PipeId = deliv_change:getval(pipeline_id, Change1),
    Change3Id = deliv_change:getval(id, Change3),
    {ok, LatestChangeset} = deliv_changeset:latest_closed_for_pipeline(PipeId),
    LatestChangeId = deliv_changeset:getval(latest_change_id, LatestChangeset),
    ?assertEqual(Change3Id, LatestChangeId).

latest_closed_for_pipeline_returns_undefined_if_no_changeset([_,_,_]) ->
    PipeId = 1,
    hoax:mock(deliv_db,
              ?expect(qfetch,
                      ?withArgs([deliv_changeset, latest_closed_for_pipeline, [PipeId]]),
                      ?andReturn([]))),

    ?assertEqual({ok, undefined}, deliv_changeset:latest_closed_for_pipeline(PipeId)).

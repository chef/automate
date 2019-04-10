-module(deliv_stage_run_integration_tests).

-include_lib("hoax/include/hoax.hrl").

-compile(export_all).

-define(assertTimestampIsApproximatelyCurrentTime(ColumnName, StageRun),
        ((fun () ->
              Timestamp = chef_utils:trunc_timestamp(deliv_stage_run:getval(ColumnName, StageRun)),
              TimestampInSeconds = calendar:datetime_to_gregorian_seconds(Timestamp),
              Now = calendar:universal_time(),
              NowInSeconds = calendar:datetime_to_gregorian_seconds(Now),

              case NowInSeconds - TimestampInSeconds < 5 of
                  true -> ok;
                  _ -> erlang:error({assertTimestampIsApproximatelyCurrentTime_failed,
                                     [{module, ?MODULE},
                                      {line, ?LINE},
                                      {timestamp, Timestamp},
                                      {current_time, Now}]})
              end
        end)())).

insert_fixture_test_() ->
    hoax:parameterized_fixture(?MODULE, "insert_", setup, teardown).

start_fixture_test_() ->
    hoax:parameterized_fixture(?MODULE, "start_", setup, teardown).

finish_fixture_test_() ->
    hoax:parameterized_fixture(?MODULE, "finish_", setup, teardown).

update_fixture_test_() ->
    hoax:parameterized_fixture(?MODULE, "update_", setup, teardown).

current_pipeline_when_merged_change_fixture_test_() ->
    hoax:parameterized_fixture(?MODULE, "current_pipeline_when_", setup, teardown).

two_changes_current_pipeline_when_merged_change_fixture_test_() ->
    hoax:parameterized_fixture(?MODULE, "two_changes_", setup_two_changes, teardown).

three_changes_current_pipeline_when_merged_change_fixture_test_() ->
    hoax:parameterized_fixture(?MODULE, "three_changes_", setup_three_changes, teardown).

two_projects_current_pipeline_when_merged_change_fixture_test_() ->
    hoax:parameterized_fixture(?MODULE, "two_projects_", setup_two_changes_two_projects, teardown).

basic_setup() ->
    error_logger:tty(false),
    application:start(gproc),
    eu_database:setup().

setup() ->
    basic_setup(),
    eu_data:with_enterprise(<<"deliv_change_test_enterprise">>,
       eu_data:with_organization(<<"deliv_change_test_organization">>,
        eu_data:with_project(<<"deliv_change_test_project">>,
          eu_data:with_pipeline(<<"master">>,
            fun(Enterprise, Organization, Project, Pipeline) ->
                User = eu_data:fetch_or_create_user(Enterprise, <<"deliv_change_eunit_user">>),
                Change = setup_change(Enterprise, Organization, Project, Pipeline, User, "not_master"),
                {Enterprise, Change, User}
            end)))).

setup_two_changes() ->
    basic_setup(),
    eu_data:with_enterprise(<<"deliv_change_test_enterprise">>,
       eu_data:with_organization(<<"deliv_change_test_organization">>,
        eu_data:with_project(<<"deliv_change_test_project">>,
          eu_data:with_pipeline(<<"master">>,
            fun(Enterprise, Organization, Project, Pipeline) ->
                User = eu_data:fetch_or_create_user(Enterprise, <<"deliv_change_eunit_user">>),
                ChangeOne = setup_change(Enterprise, Organization, Project, Pipeline, User, "not_master_1"),
                ChangeTwo = setup_change(Enterprise, Organization, Project, Pipeline, User, "not_master_2"),
                {Enterprise, ChangeOne, ChangeTwo, User}
            end)))).

setup_three_changes() ->
    basic_setup(),
    eu_data:with_enterprise(<<"deliv_change_test_enterprise">>,
       eu_data:with_organization(<<"deliv_change_test_organization">>,
        eu_data:with_project(<<"deliv_change_test_project">>,
          eu_data:with_pipeline(<<"master">>,
            fun(Enterprise, Organization, Project, Pipeline) ->
                User = eu_data:fetch_or_create_user(Enterprise, <<"deliv_change_eunit_user">>),
                ChangeOne = setup_change(Enterprise, Organization, Project, Pipeline, User, "not_master_1"),
                ChangeTwo = setup_change(Enterprise, Organization, Project, Pipeline, User,"not_master_2"),
                ChangeThree = setup_change(Enterprise, Organization, Project, Pipeline, User,"not_master_3"),
                {Enterprise, ChangeOne, ChangeTwo, ChangeThree, User}
            end)))).

setup_change(Enterprise, Organization, Project, Pipeline, User, FeatureBranch) ->
                ChangePatchset = eu_data:create_patchset(Enterprise, User, Organization,
                                        Project, Pipeline, FeatureBranch),
                Change = eu_data:change_from_patchset(ChangePatchset),
                SubmittedAtChange = deliv_change:getval(submitted_at, Change),
                UpdatedChange = deliv_change:setvals([{submitted_at,
                                                       chef_utils:trunc_timestamp(SubmittedAtChange)}], Change),
                UpdatedChange.


setup_two_changes_two_projects() ->
    basic_setup(),
    {Ent, ChgOne, TheUser} = eu_data:with_enterprise(<<"deliv_change_test_enterprise">>,
       eu_data:with_organization(<<"deliv_change_test_organization">>,
        eu_data:with_project(<<"deliv_change_test_project">>,
          eu_data:with_pipeline(<<"master">>,
            fun(Enterprise, Organization, Project, Pipeline) ->
                User = eu_data:fetch_or_create_user(Enterprise, <<"deliv_change_eunit_user">>),
                ChangeOne = setup_change(Enterprise, Organization, Project, Pipeline, User, "not_master_1"),
                {Enterprise, ChangeOne, User}
            end)))),
    ChgTwo = eu_data:with_enterprise(<<"deliv_change_test_enterprise">>,
       eu_data:with_organization(<<"deliv_change_test_organization">>,
        eu_data:with_project(<<"deliv_change_test_project_2">>,
          eu_data:with_pipeline(<<"master">>,
            fun(Enterprise, Organization, Project, Pipeline) ->
                User = eu_data:fetch_or_create_user(Enterprise, <<"deliv_change_eunit_user">>),
                ChangeTwo = setup_change(Enterprise, Organization, Project, Pipeline, User, "not_master_1"),
                ChangeTwo
            end)))),
    {Ent, ChgOne, ChgTwo, TheUser}.

teardown(_) ->
    eu_database:teardown(),
    application:stop(gproc),
    error_logger:tty(true).

insert_should_serialize_and_fetch_should_return({_, Change, _}) ->
    ChangeId = deliv_change:getval(id, Change),
    deliv_stage_run:insert([{change_id, ChangeId},
                            {stage, <<"build">>},
                            {status, <<"idle">>}]),

    [PersistedStageRun] = deliv_stage_run:fetch_all_by_change_id(ChangeId),

    ?assertTimestampIsApproximatelyCurrentTime(created_at, PersistedStageRun),
    ?assertEqual(<<"build">>, deliv_stage_run:getval(stage,PersistedStageRun)).

current_pipeline_when_verify_change_should_return_changes({Enterprise, Change, _}) ->
    StageName = <<"verify">>,
    StageStatus = <<"idle">>,
    EntName = deliv_enterprise:getval(name, Enterprise),
    ChangeId = deliv_change:getval(id, Change),
    deliv_stage_run:insert([{change_id, ChangeId},
                            {stage, StageName},
                            {status, StageStatus}]),
    Json = deliv_stage_run:current_pipeline_status_for_enterprise(EntName),

    ?assertEqual({ok, [generate_ejson(Change, StageName, StageStatus)]}, Json).

current_pipeline_when_build_change_should_return_max_stage_run({Enterprise, Change, _}) ->
    StageName = <<"build">>,
    StageStatus = <<"idle">>,
    EntName = deliv_enterprise:getval(name, Enterprise),
    ChangeId = deliv_change:getval(id, Change),
    deliv_stage_run:insert([{change_id, ChangeId},
                            {stage, <<"verify">>},
                            {status, <<"passed">>}]),
    deliv_stage_run:insert([{change_id, ChangeId},
                            {stage, StageName},
                            {status, StageStatus}]),
    Json = deliv_stage_run:current_pipeline_status_for_enterprise(EntName),

    ?assertEqual({ok, [generate_ejson(Change, StageName, StageStatus)]}, Json).

current_pipeline_when_acceptance_change_should_return_max_stage_run({Enterprise, Change, _}) ->
    StageName = <<"acceptance">>,
    StageStatus = <<"idle">>,
    EntName = deliv_enterprise:getval(name, Enterprise),
    ChangeId = deliv_change:getval(id, Change),
    deliv_changeset:add_to_changeset(Change, []),

    {ok, UpdatedChange} = deliv_change:fetch_by_id(ChangeId),

    deliv_stage_run:insert([{change_id, ChangeId},
                            {stage, <<"verify">>},
                            {status, <<"passed">>}]),
    deliv_stage_run:insert([{change_id, ChangeId},
                            {stage, <<"build">>},
                            {status, <<"passed">>}]),
    deliv_stage_run:insert([{change_id, ChangeId},
                            {stage, StageName},
                            {status, StageStatus}]),
    Json = deliv_stage_run:current_pipeline_status_for_enterprise(EntName),

    ?assertEqual({ok, [generate_ejson(UpdatedChange, StageName, StageStatus)]}, Json).

current_pipeline_when_union_change_should_return_max_stage_run({Enterprise, Change, User}) ->
    StageName = <<"union">>,
    StageStatus = <<"idle">>,
    EntName = deliv_enterprise:getval(name, Enterprise),
    ChangeId = deliv_change:getval(id, Change),
    Username = deliv_user:getval(name, User),
    deliv_changeset:add_to_changeset(Change, []),
    deliv_changeset:close_changeset(ChangeId, User),
    deliv_stage_run:insert([{change_id, ChangeId},
                            {stage, <<"verify">>},
                            {status, <<"passed">>}]),
    deliv_stage_run:insert([{change_id, ChangeId},
                            {stage, <<"build">>},
                            {status, <<"passed">>}]),
    deliv_stage_run:insert([{change_id, ChangeId},
                            {stage, <<"acceptance">>},
                            {status, <<"passed">>}]),
    deliv_stage_run:insert([{change_id, ChangeId},
                            {stage, StageName},
                            {status, StageStatus}]),
    Json = deliv_stage_run:current_pipeline_status_for_enterprise(EntName),

    {ok, UpdatedChange} = deliv_change:fetch_by_id(ChangeId),

    ?assertEqual({ok, [generate_urd_ejson(UpdatedChange, StageName, StageStatus, Username)]}, Json).

current_pipeline_when_rehearsal_change_should_return_max_stage_run({Enterprise, Change, User}) ->
    StageName = <<"rehearsal">>,
    StageStatus = <<"idle">>,
    EntName = deliv_enterprise:getval(name, Enterprise),
    ChangeId = deliv_change:getval(id, Change),
    Username = deliv_user:getval(name, User),
    deliv_changeset:add_to_changeset(Change, []),
    deliv_changeset:close_changeset(ChangeId, User),
    deliv_stage_run:insert([{change_id, ChangeId},
                            {stage, <<"verify">>},
                            {status, <<"passed">>}]),
    deliv_stage_run:insert([{change_id, ChangeId},
                            {stage, <<"build">>},
                            {status, <<"passed">>}]),
    deliv_stage_run:insert([{change_id, ChangeId},
                            {stage, <<"acceptance">>},
                            {status, <<"passed">>}]),
    deliv_stage_run:insert([{change_id, ChangeId},
                            {stage, <<"union">>},
                            {status, <<"passed">>}]),
    deliv_stage_run:insert([{change_id, ChangeId},
                            {stage, StageName},
                            {status, StageStatus}]),
    Json = deliv_stage_run:current_pipeline_status_for_enterprise(EntName),

    {ok, UpdatedChange} = deliv_change:fetch_by_id(ChangeId),

    ?assertEqual({ok, [generate_urd_ejson(UpdatedChange, StageName, StageStatus, Username)]}, Json).

current_pipeline_when_delivered_change_should_return_max_stage_run({Enterprise, Change, User}) ->
    StageName = <<"delivered">>,
    StageStatus = <<"idle">>,
    EntName = deliv_enterprise:getval(name, Enterprise),
    ChangeId = deliv_change:getval(id, Change),
    Username = deliv_user:getval(name, User),
    deliv_changeset:add_to_changeset(Change, []),
    deliv_changeset:close_changeset(ChangeId, User),
    deliv_stage_run:insert([{change_id, ChangeId},
                            {stage, <<"verify">>},
                            {status, <<"passed">>}]),
    deliv_stage_run:insert([{change_id, ChangeId},
                            {stage, <<"build">>},
                            {status, <<"passed">>}]),
    deliv_stage_run:insert([{change_id, ChangeId},
                            {stage, <<"acceptance">>},
                            {status, <<"passed">>}]),
    deliv_stage_run:insert([{change_id, ChangeId},
                            {stage, <<"union">>},
                            {status, <<"passed">>}]),
    deliv_stage_run:insert([{change_id, ChangeId},
                            {stage, <<"rehearsal">>},
                            {status, <<"passed">>}]),
    deliv_stage_run:insert([{change_id, ChangeId},
                            {stage, StageName},
                            {status, StageStatus}]),
    Json = deliv_stage_run:current_pipeline_status_for_enterprise(EntName),

    {ok, UpdatedChange} = deliv_change:fetch_by_id(ChangeId),

    ?assertEqual({ok, [generate_urd_ejson(UpdatedChange, StageName, StageStatus, Username)]}, Json).


two_changes_one_in_delivered_one_in_build_returns_correct_ejson({Enterprise,ChangeOne,ChangeTwo,User}) ->
    StageName = <<"delivered">>,
    StageStatus = <<"idle">>,
    EntName = deliv_enterprise:getval(name, Enterprise),
    ChangeOneId = deliv_change:getval(id, ChangeOne),
    ChangeTwoId = deliv_change:getval(id, ChangeTwo),
    Username = deliv_user:getval(name, User),
    deliv_changeset:add_to_changeset(ChangeOne, []),
    deliv_changeset:close_changeset(ChangeOneId, User),
    deliv_stage_run:insert([{change_id, ChangeOneId},
                            {stage, <<"verify">>},
                            {status, <<"passed">>}]),
    deliv_stage_run:insert([{change_id, ChangeOneId},
                            {stage, <<"build">>},
                            {status, <<"passed">>}]),
    deliv_stage_run:insert([{change_id, ChangeOneId},
                            {stage, <<"acceptance">>},
                            {status, <<"passed">>}]),
    deliv_stage_run:insert([{change_id, ChangeOneId},
                            {stage, <<"union">>},
                            {status, <<"passed">>}]),
    deliv_stage_run:insert([{change_id, ChangeOneId},
                            {stage, <<"rehearsal">>},
                            {status, <<"passed">>}]),
    deliv_stage_run:insert([{change_id, ChangeOneId},
                            {stage, StageName},
                            {status, StageStatus}]),
    deliv_stage_run:insert([{change_id, ChangeTwoId},
                            {stage, <<"verify">>},
                            {status, <<"passed">>}]),
    deliv_stage_run:insert([{change_id, ChangeTwoId},
                            {stage, <<"build">>},
                            {status, <<"passed">>}]),
    {ok, UpdatedChangeOne} = deliv_change:fetch_by_id(ChangeOneId),

    Json = deliv_stage_run:current_pipeline_status_for_enterprise(EntName),
    %% The query returns based on sort order of change ids
    ExpectedEjson =[Ejson || {_, Ejson} <- lists:sort([
                    {binary_to_list(ChangeTwoId), generate_ejson(ChangeTwo, <<"build">>, <<"passed">>)},
                    {binary_to_list(ChangeOneId), generate_urd_ejson(UpdatedChangeOne, StageName, StageStatus, Username)}
                   ])],

    ?assertEqual({ok, ExpectedEjson}, Json).

three_changes_one_in_acceptance_failed_included_change_failed_twice_in_acceptance_rerun_should_return_one_included_change({Enterprise,ChangeOne,ChangeTwo,ChangeThree,_User}) ->
    EntName = deliv_enterprise:getval(name, Enterprise),
    ChangeOneId = deliv_change:getval(id, ChangeOne),
    ChangeTwoId = deliv_change:getval(id, ChangeTwo),
    ChangeThreeId = deliv_change:getval(id, ChangeThree),
    deliv_changeset:add_to_changeset(ChangeOne, []),
    deliv_changeset:add_to_changeset(ChangeTwo, []),
    deliv_changeset:add_to_changeset(ChangeThree, []),
    deliv_stage_run:insert([{change_id, ChangeOneId},
                            {stage, <<"acceptance">>},
                            {status, <<"failed">>}]),
    deliv_stage_run:insert([{change_id, ChangeTwoId},
                            {stage, <<"acceptance">>},
                            {status, <<"failed">>}]),
    deliv_stage_run:insert([{change_id, ChangeThreeId},
                            {stage, <<"acceptance">>},
                            {status, <<"failed">>}]),
    {ok, UpdatedChangeOne} = deliv_change:fetch_by_id(ChangeOneId),
    {ok, UpdatedChangeTwo} = deliv_change:fetch_by_id(ChangeTwoId),
    {ok, UpdatedChangeThree} = deliv_change:fetch_by_id(ChangeThreeId),

    ActualEjson = deliv_stage_run:current_pipeline_status_for_enterprise(EntName),
    ExpectedEjson =[Ejson || {_, Ejson} <- lists:sort([
                    {binary_to_list(ChangeThreeId), generate_ejson_with_changes([UpdatedChangeThree, UpdatedChangeOne, UpdatedChangeTwo], <<"acceptance">>, <<"failed">>, null)}
                   ])],

    ?assertEqual({ok, ExpectedEjson}, ActualEjson).

two_changes_one_in_union_one_failed_and_passed_in_acceptance({Enterprise,ChangeOne,ChangeTwo,User}) ->
    EntName = deliv_enterprise:getval(name, Enterprise),
    ChangeOneId = deliv_change:getval(id, ChangeOne),
    ChangeTwoId = deliv_change:getval(id, ChangeTwo),
    deliv_stage_run:insert([{change_id, ChangeOneId},
                            {stage, <<"verify">>},
                            {status, <<"passed">>}]),
    deliv_stage_run:insert([{change_id, ChangeOneId},
                            {stage, <<"build">>},
                            {status, <<"failed">>}]),
    deliv_stage_run:insert([{change_id, ChangeOneId},
                            {stage, <<"acceptance">>},
                            {status, <<"failed">>}]),
    deliv_stage_run:insert([{change_id, ChangeOneId},
                            {stage, <<"acceptance">>},
                            {status, <<"passed">>}]),
    deliv_stage_run:insert([{change_id, ChangeTwoId},
                            {stage, <<"verify">>},
                            {status, <<"passed">>}]),
    deliv_stage_run:insert([{change_id, ChangeTwoId},
                            {stage, <<"build">>},
                            {status, <<"passed">>}]),
    deliv_stage_run:insert([{change_id, ChangeTwoId},
                            {stage, <<"acceptance">>},
                            {status, <<"passed">>}]),
    deliv_stage_run:insert([{change_id, ChangeTwoId},
                            {stage, <<"union">>},
                            {status, <<"passed">>}]),
    deliv_changeset:add_to_changeset(ChangeOne, []),
    deliv_changeset:add_to_changeset(ChangeTwo, []),

    deliv_changeset:close_changeset(ChangeTwoId, User),
    UserName = deliv_user:getval(name, User),

    {ok, UpdatedChangeOne} = deliv_change:fetch_by_id(ChangeOneId),
    {ok, UpdatedChangeTwo} = deliv_change:fetch_by_id(ChangeTwoId),

    ActualEjson = deliv_stage_run:current_pipeline_status_for_enterprise(EntName),

    ExpectedEjson = [Ejson || {_, Ejson} <- lists:sort([
                    {binary_to_list(ChangeTwoId), generate_ejson_with_changes([UpdatedChangeTwo, UpdatedChangeOne], <<"union">>, <<"passed">>, UserName)}
                    ])],

    ?assertEqual({ok, ExpectedEjson}, ActualEjson).

two_changes_one_in_build_failed_one_in_acceptance_passed_correct_ejson({Enterprise,ChangeOne,ChangeTwo,_User}) ->
    EntName = deliv_enterprise:getval(name, Enterprise),
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

    {ok, FailedChange} = deliv_change:fetch_by_id(ChangeOneId),
    UpdatedChangeOne = deliv_change:setvals([{merge_sha, <<"foo">>}], FailedChange),
    deliv_change:update(UpdatedChangeOne),
    deliv_changeset:add_to_changeset(ChangeTwo, []),
    {ok, UpdatedChangeTwo} = deliv_change:fetch_by_id(ChangeTwoId),

    Json = deliv_stage_run:current_pipeline_status_for_enterprise(EntName),
    %% The query returns based on sort order of change ids
    ExpectedEjson = [Ejson || {_, Ejson} <- lists:sort([
                        {binary_to_list(ChangeTwoId), generate_ejson_with_changes_in_diff_stages([
                          {UpdatedChangeTwo, <<"acceptance">>, <<"passed">>},
                          {UpdatedChangeOne, <<"build">>, <<"failed">> }
                        ])}
                    ])],

    ?assertEqual({ok, ExpectedEjson}, Json).

two_changes_in_different_changesets_returns_correct_ejson({Enterprise, ChangeOne, ChangeTwo, User}) ->
    StageName = <<"rehearsal">>,
    StageStatus = <<"passed">>,
    EntName = deliv_enterprise:getval(name, Enterprise),
    ChangeOneId = deliv_change:getval(id, ChangeOne),
    ChangeTwoId = deliv_change:getval(id, ChangeTwo),

    deliv_changeset:add_to_changeset(ChangeOne, []),
    {ok, UpdatedChangeOne} = deliv_change:fetch_by_id(ChangeOneId),
    deliv_stage_run:insert([{change_id, ChangeOneId},
                            {stage, <<"verify">>},
                            {status, <<"passed">>}]),
    deliv_stage_run:insert([{change_id, ChangeOneId},
                            {stage, <<"build">>},
                            {status, <<"passed">>}]),
    deliv_stage_run:insert([{change_id, ChangeOneId},
                            {stage, <<"acceptance">>},
                            {status, <<"passed">>}]),
    deliv_stage_run:insert([{change_id, ChangeOneId},
                            {stage, <<"union">>},
                            {status, <<"passed">>}]),
    deliv_stage_run:insert([{change_id, ChangeOneId},
                            {stage, StageName},
                            {status, StageStatus}]),
    deliv_changeset:close_changeset(ChangeOneId, User),

    deliv_changeset:add_to_changeset(ChangeTwo, []),
    {ok, UpdatedChangeTwo} = deliv_change:fetch_by_id(ChangeTwoId),
    deliv_stage_run:insert([{change_id, ChangeTwoId},
                            {stage, <<"verify">>},
                            {status, <<"passed">>}]),
    deliv_stage_run:insert([{change_id, ChangeTwoId},
                            {stage, <<"build">>},
                            {status, <<"passed">>}]),
    deliv_stage_run:insert([{change_id, ChangeTwoId},
                            {stage, <<"acceptance">>},
                            {status, <<"idle">>}]),
    Username = deliv_user:getval(name, User),

    ActualEjson = deliv_stage_run:current_pipeline_status_for_enterprise(EntName),

    ExpectedEjson =[Ejson || {_, Ejson} <- lists:sort([
                    {binary_to_list(ChangeTwoId), generate_ejson_with_changes([UpdatedChangeTwo], <<"acceptance">>, <<"idle">>)},
                    {binary_to_list(ChangeOneId), generate_ejson_with_changes([UpdatedChangeOne], StageName, StageStatus, Username)}
                   ])],

    ?assertEqual({ok, ExpectedEjson}, ActualEjson).

two_changes_in_same_changeset_latest_change_in_rehearsal_returns_correct_ejson({Enterprise, ChangeOne, ChangeTwo, User}) ->
    StageName = <<"rehearsal">>,
    StageStatus = <<"idle">>,
    EntName = deliv_enterprise:getval(name, Enterprise),
    ChangeOneId = deliv_change:getval(id, ChangeOne),
    ChangeTwoId = deliv_change:getval(id, ChangeTwo),

    deliv_changeset:add_to_changeset(ChangeOne, []),
    deliv_changeset:add_to_changeset(ChangeTwo, []),

    deliv_changeset:close_changeset(ChangeTwoId, User),
    Username = deliv_user:getval(name, User),

    {ok, UpdatedChangeOne} = deliv_change:fetch_by_id(ChangeOneId),
    {ok, UpdatedChangeTwo} = deliv_change:fetch_by_id(ChangeTwoId),

    deliv_stage_run:insert([{change_id, ChangeOneId},
                            {stage, <<"verify">>},
                            {status, <<"passed">>}]),
    deliv_stage_run:insert([{change_id, ChangeOneId},
                            {stage, <<"build">>},
                            {status, <<"passed">>}]),
    deliv_stage_run:insert([{change_id, ChangeOneId},
                            {stage, <<"acceptance">>},
                            {status, StageStatus}]),
    deliv_stage_run:insert([{change_id, ChangeTwoId},
                            {stage, <<"verify">>},
                            {status, <<"passed">>}]),
    deliv_stage_run:insert([{change_id, ChangeTwoId},
                            {stage, <<"build">>},
                            {status, <<"passed">>}]),
    deliv_stage_run:insert([{change_id, ChangeTwoId},
                            {stage, <<"acceptance">>},
                            {status, <<"passed">>}]),
    deliv_stage_run:insert([{change_id, ChangeTwoId},
                            {stage, <<"union">>},
                            {status, <<"passed">>}]),
    deliv_stage_run:insert([{change_id, ChangeTwoId},
                            {stage, StageName},
                            {status, StageStatus}]),

    Ejson = deliv_stage_run:current_pipeline_status_for_enterprise(EntName),

    ?assertEqual({ok, [generate_ejson_with_changes([UpdatedChangeTwo, UpdatedChangeOne], StageName, StageStatus, Username)]}, Ejson).

two_changes_both_in_acceptance_returns_correct_ejson({Enterprise, ChangeOne, ChangeTwo, _}) ->
    StageName = <<"acceptance">>,
    StageStatus = <<"idle">>,
    EntName = deliv_enterprise:getval(name, Enterprise),
    ChangeOneId = deliv_change:getval(id, ChangeOne),
    ChangeTwoId = deliv_change:getval(id, ChangeTwo),

    deliv_changeset:add_to_changeset(ChangeOne, []),
    deliv_changeset:add_to_changeset(ChangeTwo, []),

    {ok, UpdatedChangeOne} = deliv_change:fetch_by_id(ChangeOneId),
    {ok, UpdatedChangeTwo} = deliv_change:fetch_by_id(ChangeTwoId),

    deliv_stage_run:insert([{change_id, ChangeOneId},
                            {stage, <<"verify">>},
                            {status, <<"passed">>}]),
    deliv_stage_run:insert([{change_id, ChangeOneId},
                            {stage, <<"build">>},
                            {status, <<"passed">>}]),
    deliv_stage_run:insert([{change_id, ChangeOneId},
                            {stage, StageName},
                            {status, <<"failed">>}]),
    deliv_stage_run:insert([{change_id, ChangeTwoId},
                            {stage, <<"verify">>},
                            {status, <<"passed">>}]),
    deliv_stage_run:insert([{change_id, ChangeTwoId},
                            {stage, <<"build">>},
                            {status, <<"passed">>}]),
    deliv_stage_run:insert([{change_id, ChangeTwoId},
                            {stage, StageName},
                            {status, StageStatus}]),

    Ejson = deliv_stage_run:current_pipeline_status_for_enterprise(EntName),

    ?assertEqual({ok, [generate_ejson_with_changes_in_diff_stages([{UpdatedChangeTwo, StageName, StageStatus}, {UpdatedChangeOne, StageName, <<"failed">>}])]}, Ejson).

two_projects_one_in_build_failed_one_in_acceptance_passed_correct_ejson({Enterprise,ChangeOne,ChangeTwo,_User}) ->
    EntName = deliv_enterprise:getval(name, Enterprise),
    ChangeOneId = deliv_change:getval(id, ChangeOne),
    ChangeTwoId = deliv_change:getval(id, ChangeTwo),
    deliv_changeset:add_to_changeset(ChangeTwo, []),

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

    {ok, UpdatedChangeTwo} = deliv_change:fetch_by_id(ChangeTwoId),

    Json = deliv_stage_run:current_pipeline_status_for_enterprise(EntName),
    %% The query returns based on sort order of change ids
    ExpectedEjson =[Ejson || {_, Ejson} <- lists:sort([
                    {binary_to_list(ChangeOneId), generate_ejson(ChangeOne, <<"build">>, <<"failed">>)},
                    {binary_to_list(ChangeTwoId), generate_ejson(UpdatedChangeTwo, <<"acceptance">>, <<"passed">>)}
                    ])],

    ?assertEqual({ok, ExpectedEjson}, Json).

generate_ejson(Change, StageName, StageStatus) ->
    generate_ejson(Change, StageName, StageStatus, []).

generate_ejson(Change, StageName, StageStatus, ChangesIncluded) ->
    {EjsonPropList} = generate_ejson_change(Change, StageName, StageStatus, null),
    {EjsonPropList ++ [{<<"includes">>, ChangesIncluded}]}.

generate_ejson_with_changes(Changes, StageName, StageStatus) ->
    generate_ejson_with_changes(Changes, StageName, StageStatus, null).

generate_ejson_with_changes([H | T], StageName, StageStatus, Username) ->
    Changes = [generate_ejson_change(Change, <<"acceptance">>, StageStatus, Username) || Change <- T],
    generate_urd_ejson(H, StageName, StageStatus, Username, Changes).

generate_urd_ejson(Change, StageName, StageStatus, Username) ->
    generate_urd_ejson(Change, StageName, StageStatus, Username, []).

generate_urd_ejson(Change, StageName, StageStatus, Username, ChangesIncluded) ->
    {EjsonPropList} = generate_ejson_change(Change, StageName, StageStatus, Username),
    {EjsonPropList ++ [{<<"includes">>, ChangesIncluded}]}.

generate_ejson_change(Change, StageName, StageStatus, Username) ->
    Scope = deliv_scopes:from_change(Change),
    {[
        {<<"id">>,deliv_change:getval(id, Change)},
        {<<"title">>,null},
        {<<"org">>,deliv_scopes:'#get'(org_name, Scope)},
        {<<"project">>,deliv_scopes:'#get'(proj_name, Scope)},
        {<<"stage">>,StageName},
        {<<"stage_status">>,StageStatus},
        {<<"submitter">>,deliv_change:getval(submitted_by, Change)},
        {<<"submitted_at">>,chef_utils:format_timestamp(deliv_change:getval(submitted_at, Change))},
        {<<"approved_by">>,null},
        {<<"delivered_by">>,Username}
     ]}.

generate_ejson_with_changes_in_diff_stages(Changes) ->
  generate_ejson_with_changes_in_diff_stages(Changes, null).

generate_ejson_with_changes_in_diff_stages([{H, StageName, StageStatus} | T], Username) ->
    Changes = [generate_ejson_change(Change, ChangeStageName, ChangeStageStatus, Username) || {Change, ChangeStageName, ChangeStageStatus} <- T],
    generate_urd_ejson(H, StageName, StageStatus, Username, Changes).

start_should_set_the_status_and_started_at({_, Change, _}) ->
    ChangeId = deliv_change:getval(id, Change),
    deliv_stage_run:insert([{change_id, ChangeId},
                            {stage, <<"build">>},
                            {status, <<"idle">>}]),

    [PersistedStageRun] = deliv_stage_run:fetch_all_by_change_id(ChangeId),

    deliv_stage_run:start(PersistedStageRun),

    [RunningStageRun] = deliv_stage_run:fetch_all_by_change_id(ChangeId),

    ?assertTimestampIsApproximatelyCurrentTime(started_at, RunningStageRun),
    ?assertEqual(<<"running">>, deliv_stage_run:getval(status, RunningStageRun)).

finish_with_failed_should_set_finished_and_finished_at({_, Change, _}) ->
    ChangeId = deliv_change:getval(id, Change),
    deliv_stage_run:insert([{change_id, ChangeId},
                            {stage, <<"build">>},
                            {status, <<"failed">>}]),

    [PersistedStageRun] = deliv_stage_run:fetch_all_by_change_id(ChangeId),

    deliv_stage_run:finish(PersistedStageRun, failed),

    [FinishedStageRun] = deliv_stage_run:fetch_all_by_change_id(ChangeId),

    ?assertTimestampIsApproximatelyCurrentTime(finished_at, FinishedStageRun),
    ?assertEqual(true, deliv_stage_run:getval(finished, FinishedStageRun)),
    ?assertEqual(<<"failed">>, deliv_stage_run:getval(status, FinishedStageRun)).

finish_with_passed_should_set_the_status_and_finished_and_finished_at({_, Change, _}) ->
    ChangeId = deliv_change:getval(id, Change),
    deliv_stage_run:insert([{change_id, ChangeId},
                            {stage, <<"build">>},
                            {status, <<"running">>}]),

    [PersistedStageRun] = deliv_stage_run:fetch_all_by_change_id(ChangeId),

    deliv_stage_run:finish(PersistedStageRun, passed),

    [FinishedStageRun] = deliv_stage_run:fetch_all_by_change_id(ChangeId),

    ?assertTimestampIsApproximatelyCurrentTime(finished_at, FinishedStageRun),
    ?assertEqual(true, deliv_stage_run:getval(finished, FinishedStageRun)),
    ?assertEqual(<<"passed">>, deliv_stage_run:getval(status, FinishedStageRun)).

update_is_successful_with_existing_timestamps({_, Change, _}) ->
    ChangeId = deliv_change:getval(id, Change),
    [StageRun] = deliv_stage_run:insert([{change_id, ChangeId},
                                         {stage, <<"build">>},
                                         {status, <<"idle">>}]),
    deliv_stage_run:start(StageRun),

    [PersistedStageRun] = deliv_stage_run:fetch_all_by_change_id(ChangeId),
    OriginalCreatedAt = deliv_stage_run:getval(created_at, PersistedStageRun),
    OriginalStartedAt = deliv_stage_run:getval(started_at, PersistedStageRun),

    {ok, UpdatedStageRun} = deliv_stage_run:finish(PersistedStageRun, passed),

    ?assertEqual(OriginalCreatedAt, deliv_stage_run:getval(created_at, UpdatedStageRun)),
    ?assertNotEqual(OriginalStartedAt, undefined),
    ?assertEqual(OriginalStartedAt, deliv_stage_run:getval(started_at, UpdatedStageRun)),
    ?assertEqual(<<"build">>, deliv_stage_run:getval(stage, UpdatedStageRun)),
    ?assertEqual(<<"passed">>, deliv_stage_run:getval(status, UpdatedStageRun)).

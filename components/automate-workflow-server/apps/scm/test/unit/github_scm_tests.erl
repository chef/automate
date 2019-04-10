-module(github_scm_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_coordinates.hrl").

-compile(export_all).

hoax_fixture_test_() ->
    hoax:fixture(?MODULE).

patchset_metadata_ejson_delegates_to_scm_local() ->
    Scope = scope,
    Patchset = patchset,
    hoax:mock(deliv_scm_local,
              ?expect(patchset_metadata_ejson,
                      ?withArgs([Scope, Patchset]))),

    github_scm:patchset_metadata_ejson(Scope, Patchset),
    ?verifyAll.

clone_url_3_delegates_to_scm_local() ->
    Scope = scope,
    Patchset = patchset,
    Username = username,
    hoax:mock(deliv_scm_local,
              ?expect(clone_url,
                      ?withArgs([Username, Patchset, Scope]))),

    github_scm:clone_url(Username, Patchset, Scope),
    ?verifyAll.

clone_url_4_delegates_to_scm_local() ->
    Scope = scope,
    Patchset = patchset,
    Username = username,
    Fips = false,
    hoax:mock(deliv_scm_local,
              ?expect(clone_url,
                      ?withArgs([Username, Patchset, Scope, Fips]))),

    github_scm:clone_url(Username, Patchset, Scope, Fips),
    ?verifyAll.

load_config_for_patchset_delegates_to_scm_local() ->
    Scope = scope,
    Patchset = patchset,
    hoax:mock(deliv_scm_local,
              ?expect(load_config_for_patchset,
                      ?withArgs([Patchset, Scope]))),

    github_scm:load_config_for_patchset(Patchset, Scope),
    ?verifyAll.

patchset_branch_delegates_to_scm_local() ->
    Scope = scope,
    Patchset = patchset,
    Change = change,
    hoax:mock(deliv_scm_local,
              ?expect(patchset_branch,
                      ?withArgs([Change, Patchset, Scope]))),

    github_scm:patchset_branch(Change, Patchset, Scope),
    ?verifyAll.

merge_feature_branch_delegates_to_scm_local_and_pushes_to_github_async() ->
    EntName = <<"Ent">>,
    OrgName = <<"Org">>,
    ProjName = <<"Proj">>,
    PipeName = <<"master">>,

    Scope = deliv_scopes:'#new_common'([{scoping_names, [EntName, OrgName, ProjName, PipeName]},
                                        {ent_name, EntName},
                                        {org_name, OrgName},
                                        {proj_name, ProjName},
                                        {pipe_name, PipeName}]),
    Coords = #proj_coordinates{ent_name = EntName,
                               org_name = OrgName,
                               proj_name = ProjName},
    Patchset = deliv_patchset:'#new'(),
    Approver = deliv_user:'#new'(),
    Msg = <<"successful local merge">>,

    hoax:mock(deliv_scm_local,
              ?expect(merge_feature_branch,
                      ?withArgs([Scope, Patchset, Approver]),
                      ?andReturn({ok, Msg}))),
    hoax:mock(scm_git_client,
              ?expect(async_force_push,
                      ?withArgs([PipeName, PipeName, Coords]),
                      ?andReturn({ok, <<"success but we ignore this">>}))),

    ?assertEqual({ok, Msg}, github_scm:merge_feature_branch(Scope, Patchset, Approver)),
    ?verifyAll.

merge_feature_branch_delegates_to_scm_local_and_pushes_to_github_async_ignores_failure() ->
    EntName = <<"Ent">>,
    OrgName = <<"Org">>,
    ProjName = <<"Proj">>,
    PipeName = <<"master">>,

    Scope = deliv_scopes:'#new_common'([{scoping_names, [EntName, OrgName, ProjName, PipeName]},
                                        {ent_name, EntName},
                                        {org_name, OrgName},
                                        {proj_name, ProjName},
                                        {pipe_name, PipeName}]),
    Coords = #proj_coordinates{ent_name = EntName,
                               org_name = OrgName,
                               proj_name = ProjName},
    Patchset = deliv_patchset:'#new'(),
    Approver = deliv_user:'#new'(),
    Msg = <<"successful local merge">>,

    hoax:mock(deliv_scm_local,
              ?expect(merge_feature_branch,
                      ?withArgs([Scope, Patchset, Approver]),
                      ?andReturn({ok, Msg}))),
    hoax:mock(scm_git_client,
              ?expect(async_force_push,
                      ?withArgs([PipeName, PipeName, Coords]),
                      ?andReturn({error, <<"error but we ignore this">>}))),

    ?assertEqual({ok, Msg}, github_scm:merge_feature_branch(Scope, Patchset, Approver)),
    ?verifyAll.

merge_feature_branch_forwards_error_if_cannot_merge_locally() ->
    Scope = deliv_scopes:'#new_common'(),
    Patchset = deliv_patchset:'#new'(),
    Approver = deliv_user:'#new'(),

    hoax:expect(receive
                    deliv_scm_local:merge_feature_branch(Scope, Patchset, Approver) -> {error, whatever}
                end),

    ?assertEqual({error, whatever}, github_scm:merge_feature_branch(Scope, Patchset, Approver)),
    ?verifyAll.

delete_feature_branch_delegates_to_scm_local_and_deletes_from_github_async() ->
    EntName = <<"Ent">>,
    OrgName = <<"Org">>,
    ProjName = <<"Proj">>,
    PipeName = <<"master">>,
    ChangeId = <<"change_id">>,
    FeatBranch = <<"feature-branch">>,
    Scope = deliv_scopes:'#new_common'([{scoping_names, [EntName, OrgName, ProjName, PipeName]},
                                        {ent_name, EntName},
                                        {org_name, OrgName},
                                        {proj_name, ProjName},
                                        {pipe_name, PipeName}]),

    Coords = #proj_coordinates{ent_name = EntName,
                               org_name = OrgName,
                               proj_name = ProjName},
    Patchset = deliv_patchset:fromlist([{change_id, ChangeId}]),
    Change = deliv_change:fromlist([{id, ChangeId},
                                    {feature_branch, FeatBranch}]),

    hoax:mock(deliv_change,
              [?expect(fetch_by_id,
                       ?withArgs([ChangeId]),
                       ?andReturn({ok, Change})),
               ?expect(getval,
                       ?withArgs([feature_branch, Change]),
                       ?andReturn(FeatBranch))]),

    ScmLocalResult = {ok, <<"Git Success">>},
    hoax:mock(deliv_scm_local,
              ?expect(delete_feature_branch,
                      ?withArgs([Scope, Patchset]),
                      ?andReturn(ScmLocalResult))),

    hoax:mock(scm_git_client,
              ?expect(async_delete_branch,
                      ?withArgs([FeatBranch, Coords]),
                      ?andReturn({ok, <<"result ignored">>}))),

    ?assertEqual(ScmLocalResult, github_scm:delete_feature_branch(Scope, Patchset)),
    ?verifyAll.

delete_feature_branch_delegates_to_scm_local_and_deletes_from_github_async_ignores_error() ->
    EntName = <<"Ent">>,
    OrgName = <<"Org">>,
    ProjName = <<"Proj">>,
    PipeName = <<"master">>,
    ChangeId = <<"change_id">>,
    FeatBranch = <<"feature-branch">>,
    Scope = deliv_scopes:'#new_common'([{scoping_names, [EntName, OrgName, ProjName, PipeName]},
                                        {ent_name, EntName},
                                        {org_name, OrgName},
                                        {proj_name, ProjName},
                                        {pipe_name, PipeName}]),

    Coords = #proj_coordinates{ent_name = EntName,
                               org_name = OrgName,
                               proj_name = ProjName},
    Patchset = deliv_patchset:fromlist([{change_id, ChangeId}]),
    Change = deliv_change:fromlist([{id, ChangeId},
                                    {feature_branch, FeatBranch}]),

    hoax:mock(deliv_change, [
              ?expect(fetch_by_id,
                      ?withArgs([ChangeId]),
                      ?andReturn({ok, Change})),
              ?expect(getval,
                      ?withArgs([feature_branch, Change]),
                      ?andReturn(FeatBranch))]),

    ScmLocalResult = {ok, <<"Git Success">>},
    hoax:mock(deliv_scm_local,
              ?expect(delete_feature_branch,
                      ?withArgs([Scope, Patchset]),
                      ?andReturn(ScmLocalResult))),

    hoax:mock(scm_git_client,
              ?expect(async_delete_branch,
                      ?withArgs([FeatBranch, Coords]),
                      ?andReturn({error, <<"it failed but we don't care">>}))),

    ?assertEqual(ScmLocalResult, github_scm:delete_feature_branch(Scope, Patchset)),
    ?verifyAll.

process_new_patchset_returns_ok_msg_after_force_pushes_and_ensures_pull_request_succesfully() ->
    ChangeId = 9992,
    Patchset = deliv_patchset:fromlist([{change_id, ChangeId}]),
    ProjDir = proj_dir,
    PipeName = pipe_name,
    FeatBranchName = feat_branch_name,
    RawBranchName = raw_branch_name,
    Coords = coords,
    Msg = <<"You did it!">>,
    PRUrl = <<"https://github.com/stephen/rocks/pulls/1">>,
    LatestBranch = "olive-branch",
    PREJson = {[{<<"id">>, 32767},
                {<<"number">>, 42},
                {<<"html_url">>, PRUrl}]},

    hoax:expect(
      receive
          deliv_scm_local:process_new_patchset(Patchset, ProjDir, PipeName, FeatBranchName, RawBranchName, Coords) ->
              {ok, Msg};
          deliv_git:feature_branch(PipeName, FeatBranchName) ->
              LatestBranch;
          scm_git_client:force_push(list_to_binary(LatestBranch), FeatBranchName, Coords) ->
              {ok, whatevs};
          scm_github_rest_api:ensure_pull_request(Patchset, FeatBranchName, PipeName, Coords) ->
              {ok, PREJson}
      end,
      scm_change:save(ChangeId, 42, PRUrl)),

    ?assertEqual({ok, <<"You did it! GitHub Pull Request: https://github.com/stephen/rocks/pulls/1">>},
                 github_scm:process_new_patchset(Patchset, ProjDir, PipeName, FeatBranchName, RawBranchName, Coords)),
    ?verifyAll.

process_new_patchset_returns_ok_error_msg_no_pr_when_force_push_fails() ->
    Patchset = patchset,
    ProjDir = proj_dir,
    PipeName = pipe_name,
    FeatBranchName = feat_branch_name,
    RawBranchName = raw_branch_name,
    Coords = coords,
    Msg = <<"You did it locally!">>,
    LatestBranch = "olive-branch",
    WhyFail = <<"But not remotely.">>,

    hoax:mock(deliv_scm_local,
              ?expect(process_new_patchset,
                      ?withArgs([Patchset, ProjDir, PipeName, FeatBranchName, RawBranchName, Coords]),
                      ?andReturn({ok, Msg}))),
    hoax:mock(deliv_git,
              ?expect(feature_branch,
                      ?withArgs([PipeName, FeatBranchName]),
                      ?andReturn(LatestBranch))),
    hoax:mock(scm_git_client,
              ?expect(force_push,
                      ?withArgs([list_to_binary(LatestBranch), FeatBranchName, Coords]),
                      ?andReturn({error, {git_failed, {something, WhyFail}}}))),

    ?assertEqual({ok, <<"You did it locally! Failed to push branch to GitHub: But not remotely.">>},
                 github_scm:process_new_patchset(Patchset, ProjDir, PipeName, FeatBranchName, RawBranchName, Coords)),
    ?verifyAll.

process_new_patchset_returns_ok_error_msg_no_pr_when_force_push_fails_bc_git_cmd_fails() ->
    Patchset = patchset,
    ProjDir = proj_dir,
    PipeName = pipe_name,
    FeatBranchName = feat_branch_name,
    RawBranchName = raw_branch_name,
    Coords = coords,
    Msg = <<"You did it locally!">>,
    LatestBranch = "olive-branch",
    WhyFail = <<"But not remotely.">>,

    hoax:mock(deliv_scm_local,
              ?expect(process_new_patchset,
                      ?withArgs([Patchset, ProjDir, PipeName, FeatBranchName, RawBranchName, Coords]),
                      ?andReturn({ok, Msg}))),
    hoax:mock(deliv_git,
              ?expect(feature_branch,
                      ?withArgs([PipeName, FeatBranchName]),
                      ?andReturn(LatestBranch))),
    hoax:mock(scm_git_client,
              ?expect(force_push,
                      ?withArgs([list_to_binary(LatestBranch), FeatBranchName, Coords]),
                      ?andReturn({error, {git_cmd_failed, {something, something_else, WhyFail}}}))),

    ?assertEqual({ok, <<"You did it locally! Failed to push branch to GitHub: But not remotely.">>},
                 github_scm:process_new_patchset(Patchset, ProjDir, PipeName, FeatBranchName, RawBranchName, Coords)),
    ?verifyAll.

process_new_patchset_returns_ok_error_msg_no_pr_when_force_push_fails_bc_git_cmd_error() ->
    Patchset = patchset,
    ProjDir = proj_dir,
    PipeName = pipe_name,
    FeatBranchName = feat_branch_name,
    RawBranchName = raw_branch_name,
    Coords = coords,
    Msg = <<"You did it locally!">>,
    LatestBranch = "olive-branch",
    WhyFail = <<"But not remotely.">>,

    hoax:mock(deliv_scm_local,
              ?expect(process_new_patchset,
                      ?withArgs([Patchset, ProjDir, PipeName, FeatBranchName, RawBranchName, Coords]),
                      ?andReturn({ok, Msg}))),
    hoax:mock(deliv_git,
              ?expect(feature_branch,
                      ?withArgs([PipeName, FeatBranchName]),
                      ?andReturn(LatestBranch))),
    hoax:mock(scm_git_client,
              ?expect(force_push,
                      ?withArgs([list_to_binary(LatestBranch), FeatBranchName, Coords]),
                      ?andReturn({error, {git_cmd_error, {something, something_else, WhyFail}}}))),

    ?assertEqual({ok, <<"You did it locally! Failed to push branch to GitHub: But not remotely.">>},
                 github_scm:process_new_patchset(Patchset, ProjDir, PipeName, FeatBranchName, RawBranchName, Coords)),
    ?verifyAll.

process_new_patchset_returns_ok_error_msg_no_pr_when_force_push_for_any_other_error() ->
    Patchset = patchset,
    ProjDir = proj_dir,
    PipeName = pipe_name,
    FeatBranchName = feat_branch_name,
    RawBranchName = raw_branch_name,
    Coords = coords,
    Msg = <<"You did it locally!">>,
    LatestBranch = "olive-branch",

    hoax:mock(deliv_scm_local,
              ?expect(process_new_patchset,
                      ?withArgs([Patchset, ProjDir, PipeName, FeatBranchName, RawBranchName, Coords]),
                      ?andReturn({ok, Msg}))),
    hoax:mock(deliv_git,
              ?expect(feature_branch,
                      ?withArgs([PipeName, FeatBranchName]),
                      ?andReturn(LatestBranch))),
    hoax:mock(scm_git_client,
              ?expect(force_push,
                      ?withArgs([list_to_binary(LatestBranch), FeatBranchName, Coords]),
                      ?andReturn({error, whateverrrrrrrrr}))),

    ?assertEqual({ok, <<"You did it locally! Failed to push branch to GitHub.">>},
                 github_scm:process_new_patchset(Patchset, ProjDir, PipeName, FeatBranchName, RawBranchName, Coords)),
    ?verifyAll.

process_new_patchset_returns_ok_with_a_msg_if_pr_already_exists() ->
    Patchset = patchset,
    ProjDir = proj_dir,
    PipeName = pipe_name,
    FeatBranchName = feat_branch_name,
    RawBranchName = raw_branch_name,
    Coords = coords,
    Msg = <<"You did it!">>,
    LatestBranch = "olive-branch",
    PRMsg = <<"That PR already exists, silly.">>,

    hoax:mock(deliv_scm_local,
              ?expect(process_new_patchset,
                      ?withArgs([Patchset, ProjDir, PipeName, FeatBranchName, RawBranchName, Coords]),
                      ?andReturn({ok, Msg}))),
    hoax:mock(deliv_git,
              ?expect(feature_branch,
                      ?withArgs([PipeName, FeatBranchName]),
                      ?andReturn(LatestBranch))),
    hoax:mock(scm_git_client,
              ?expect(force_push,
                      ?withArgs([list_to_binary(LatestBranch), FeatBranchName, Coords]),
                      ?andReturn({ok, whatevs}))),
    hoax:mock(scm_github_rest_api,
              ?expect(ensure_pull_request,
                      ?withArgs([Patchset, FeatBranchName, PipeName, Coords]),
                      ?andReturn({exists, PRMsg}))),

    ?assertEqual({ok, <<"You did it! That PR already exists, silly.">>},
                 github_scm:process_new_patchset(Patchset, ProjDir, PipeName, FeatBranchName, RawBranchName, Coords)),
    ?verifyAll.

process_new_patchset_returns_ok_with_error_msg_if_creating_pr_fails() ->
    Patchset = patchset,
    ProjDir = proj_dir,
    PipeName = pipe_name,
    FeatBranchName = feat_branch_name,
    RawBranchName = raw_branch_name,
    Coords = coords,
    Msg = <<"You did it!">>,
    LatestBranch = "olive-branch",
    PRMsg = <<"Geee, Bob, you broke it.">>,

    hoax:mock(deliv_scm_local,
              ?expect(process_new_patchset,
                      ?withArgs([Patchset, ProjDir, PipeName, FeatBranchName, RawBranchName, Coords]),
                      ?andReturn({ok, Msg}))),
    hoax:mock(deliv_git,
              ?expect(feature_branch,
                      ?withArgs([PipeName, FeatBranchName]),
                      ?andReturn(LatestBranch))),
    hoax:mock(scm_git_client,
              ?expect(force_push,
                      ?withArgs([list_to_binary(LatestBranch), FeatBranchName, Coords]),
                      ?andReturn({ok, whatevs}))),
    hoax:mock(scm_github_rest_api,
              ?expect(ensure_pull_request,
                      ?withArgs([Patchset, FeatBranchName, PipeName, Coords]),
                      ?andReturn({error, PRMsg}))),

    ?assertEqual({ok, <<"You did it! Geee, Bob, you broke it.">>},
                 github_scm:process_new_patchset(Patchset, ProjDir, PipeName, FeatBranchName, RawBranchName, Coords)),
    ?verifyAll.

pull_request_description_returns_expected_text_test() ->
    ?assertEqual(<<"GitHub Pull Request (#42)">>, github_scm:pull_request_description(42)).

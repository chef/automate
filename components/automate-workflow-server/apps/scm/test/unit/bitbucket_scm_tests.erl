-module(bitbucket_scm_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_coordinates.hrl").

-compile(export_all).

hoax_fixture_test_() ->
    hoax:fixture(?MODULE).

patchset_metadata_ejson_calls_scm_local() ->
    Scope = deliv_scopes:'#new_common'(),
    Patchset = deliv_patchset:'#new'(),

    hoax:mock(deliv_scm_local,
        ?expect(patchset_metadata_ejson,
                ?withArgs([Scope, Patchset]))),

    bitbucket_scm:patchset_metadata_ejson(Scope, Patchset),

    ?verifyAll.

clone_url_3_calls_scm_local() ->
    Username = <<"testuser">>,
    Scope = deliv_scopes:'#new_common'(),
    Patchset = deliv_patchset:'#new'(),

    hoax:mock(deliv_scm_local,
        ?expect(clone_url,
                ?withArgs([Username, Scope, Patchset]))),

    bitbucket_scm:clone_url(Username, Scope, Patchset),

    ?verifyAll.

clone_url_4_calls_scm_local() ->
    Username = <<"testuser">>,
    Scope = deliv_scopes:'#new_common'(),
    Patchset = deliv_patchset:'#new'(),
    Fips = false,

    hoax:mock(deliv_scm_local,
        ?expect(clone_url,
                ?withArgs([Username, Scope, Patchset, Fips]))),

    bitbucket_scm:clone_url(Username, Scope, Patchset, Fips),

    ?verifyAll.

load_config_for_patchset_calls_scm_local() ->
    Scope = deliv_scopes:'#new_common'(),
    Patchset = deliv_patchset:'#new'(),

    hoax:mock(deliv_scm_local,
        ?expect(load_config_for_patchset,
                ?withArgs([Scope, Patchset]))),

    bitbucket_scm:load_config_for_patchset(Scope, Patchset),

    ?verifyAll.

patchset_branch_calls_scm_local() ->
    Change = deliv_change:'#new'(),
    Scope = deliv_scopes:'#new_common'(),
    Patchset = deliv_patchset:'#new'(),

    hoax:mock(deliv_scm_local,
        ?expect(patchset_branch,
                ?withArgs([Change, Scope, Patchset]))),

    bitbucket_scm:patchset_branch(Change, Scope, Patchset),

    ?verifyAll.

merge_feature_branch_calls_scm_local_and_successfully_pushes_to_bitbucket() ->
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

    Msg = <<"¡Bueno!">>,
    hoax:mock(deliv_scm_local,
        ?expect(merge_feature_branch,
                ?withArgs([Scope, Patchset, Approver]),
                ?andReturn({ok, Msg}))),

    hoax:mock(scm_git_client,
              ?expect(async_force_push,
                      ?withArgs([PipeName, PipeName, Coords]),
                      ?andReturn({ok, <<"success">>}))),

    Result = bitbucket_scm:merge_feature_branch(Scope, Patchset, Approver),
    ?assertEqual({ok, Msg}, Result),
    ?verifyAll.

merge_feature_branch_calls_scm_local_and_fails_to_push_bitbucket() ->
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

    Msg = <<"¡Bueno!">>,
    hoax:mock(deliv_scm_local,
              ?expect(merge_feature_branch,
                      ?withArgs([Scope, Patchset, Approver]),
                      ?andReturn({ok, Msg}))),

    ErrMsg = <<"¡No Bueno!">>,
    hoax:mock(scm_git_client,
              ?expect(async_force_push,
                      ?withArgs([PipeName, PipeName, Coords]),
                      ?andReturn({error, ErrMsg}))),

    Result = bitbucket_scm:merge_feature_branch(Scope, Patchset, Approver),
    ?assertEqual({ok, Msg}, Result),
    ?verifyAll.

merge_feature_branch_forwards_error_if_cannot_merge_locally() ->
    Scope = deliv_scopes:'#new_common'(),
    Patchset = deliv_patchset:'#new'(),
    Approver = deliv_user:'#new'(),

    hoax:expect(receive
                    deliv_scm_local:merge_feature_branch(Scope, Patchset, Approver) -> {error, whatever}
                end),

    ?assertEqual({error, whatever}, bitbucket_scm:merge_feature_branch(Scope, Patchset, Approver)),
    ?verifyAll.


delete_feature_branch_call_scm_local_given_bitbucket_delete_ok_return_scm_local() ->
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
                      ?andReturn({ok, <<"Success">>}))),

    ?assertEqual(ScmLocalResult, bitbucket_scm:delete_feature_branch(Scope, Patchset)),
    ?verifyAll.

delete_feature_branch_call_scm_local_given_bitbucket_delete_failure_return_scm_local() ->
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
                      ?andReturn({error, <<"it failed">>}))),

    ?assertEqual(ScmLocalResult, bitbucket_scm:delete_feature_branch(Scope, Patchset)),
    ?verifyAll.

process_new_patchset_returns_ok() ->
    Patchset = deliv_patchset:fromlist([{change_id, <<"change_id">>}]),
    ProjDir = <<"testdir">>,
    PipeName = <<"testpipe">>,
    FeatBranchName = <<"testfeatbranch">>,
    RawBranchName = <<"testrawbranch">>,
    SourceBranch = <<"SourceBranch">>,
    PullRequestUrl = <<"http://link/to/pullrequest">>,
    PullRequestId = 1,
    PullRequestJson = {[{
                        <<"id">>, PullRequestId},
                        {<<"links">>,
                        {[{<<"self">>,
                           [{[{<<"href">>,
                               PullRequestUrl}]}]}]}}]},
    Coords = #proj_coordinates{ent_name = <<"testent">>, org_name = <<"testorg">>, proj_name = <<"testproj">>},
    ChangeId = deliv_patchset:getval(change_id, Patchset),
    ChangeBitbucketMetadata = {scm_change, ChangeId,
                               PullRequestId,
                               PullRequestUrl},


    hoax:mock(deliv_scm_local,
              ?expect(process_new_patchset,
                      ?withArgs([Patchset, ProjDir, PipeName, FeatBranchName, RawBranchName, Coords]),
                      ?andReturn({ok, <<"processed\n">>}))),

    hoax:mock(deliv_git,
              ?expect(feature_branch,
                      ?withArgs([PipeName, FeatBranchName]),
                      ?andReturn(SourceBranch))),

    hoax:mock(scm_git_client,
              ?expect(force_push,
                      ?withArgs([SourceBranch, FeatBranchName, Coords]),
                      ?andReturn({ok, <<"Term">>}))),

    hoax:mock(scm_bitbucket_rest_api,
              ?expect(ensure_pull_request,
                      ?withArgs([Patchset, FeatBranchName, PipeName, Coords]),
                      ?andReturn({ok, PullRequestJson}))),

    hoax:mock(scm_change,
                ?expect(save,
                        ?withArgs([ChangeId, PullRequestId, PullRequestUrl]),
                        ?andReturn({ok, ChangeBitbucketMetadata}))),

    Result = bitbucket_scm:process_new_patchset(Patchset, ProjDir, PipeName, FeatBranchName, RawBranchName, Coords),

    ?assertEqual({ok, <<"processed\nBitbucket Pull Request: ", PullRequestUrl/binary>>}, Result),
    ?verifyAll.

process_new_patchset_returns_ok_when_pull_request_fails() ->
    Patchset = deliv_patchset:'#new'(),
    ProjDir = <<"testdir">>,
    PipeName = <<"testpipe">>,
    FeatBranchName = <<"testfeatbranch">>,
    RawBranchName = <<"testrawbranch">>,
    SourceBranch = <<"SourceBranch">>,
    Why = <<"Why the pull request failed.">>,
    Coords = #proj_coordinates{ent_name = <<"testent">>, org_name = <<"testorg">>, proj_name = <<"testproj">>},

    hoax:mock(deliv_scm_local,
              ?expect(process_new_patchset,
                      ?withArgs([Patchset, ProjDir, PipeName, FeatBranchName, RawBranchName, Coords]),
                      ?andReturn({ok, <<"processed\n">>}))),

    hoax:mock(deliv_git,
              ?expect(feature_branch,
                      ?withArgs([PipeName, FeatBranchName]),
                      ?andReturn(SourceBranch))),

    hoax:mock(scm_git_client,
              ?expect(force_push,
                      ?withArgs([SourceBranch, FeatBranchName, Coords]),
                      ?andReturn({ok, <<"Term">>}))),

    hoax:mock(scm_bitbucket_rest_api,
              ?expect(ensure_pull_request,
                      ?withArgs([Patchset, FeatBranchName, PipeName, Coords]),
                      ?andReturn({error, Why}))),

    Result = bitbucket_scm:process_new_patchset(Patchset, ProjDir, PipeName, FeatBranchName, RawBranchName, Coords),

    ?assertEqual({ok, <<"processed\n", Why/binary>>}, Result),
    ?verifyAll.

process_new_patchset_returns_ok_when_fails_to_push_to_bitbucket_error_git_failed() ->
    Patchset = deliv_patchset:'#new'(),
    ProjDir = <<"testdir">>,
    PipeName = <<"testpipe">>,
    FeatBranchName = <<"testfeatbranch">>,
    RawBranchName = <<"testrawbranch">>,
    SourceBranch = <<"SourceBranch">>,
    Coords = #proj_coordinates{ent_name = <<"testent">>, org_name = <<"testorg">>, proj_name = <<"testproj">>},

    hoax:mock(deliv_scm_local,
              ?expect(process_new_patchset,
                      ?withArgs([Patchset, ProjDir, PipeName, FeatBranchName, RawBranchName, Coords]),
                      ?andReturn({ok, <<"processed\n">>}))),

    hoax:mock(deliv_git,
              ?expect(feature_branch,
                      ?withArgs([PipeName, FeatBranchName]),
                      ?andReturn(SourceBranch))),

    hoax:mock(scm_git_client,
              ?expect(force_push,
                      ?withArgs([SourceBranch, FeatBranchName, Coords]),
                      ?andReturn({error, {git_failed, {status, <<"output">>}}}))),

    Result = bitbucket_scm:process_new_patchset(Patchset, ProjDir, PipeName, FeatBranchName, RawBranchName, Coords),

    ?assertEqual({ok, <<"processed\nFailed to push branch to Bitbucket: output">>}, Result),
    ?verifyAll.

process_new_patchset_returns_ok_when_fails_to_push_to_bitbucket_error_git_cmd_failed() ->
    Patchset = deliv_patchset:'#new'(),
    ProjDir = <<"testdir">>,
    PipeName = <<"testpipe">>,
    FeatBranchName = <<"testfeatbranch">>,
    RawBranchName = <<"testrawbranch">>,
    SourceBranch = <<"SourceBranch">>,
    Coords = #proj_coordinates{ent_name = <<"testent">>, org_name = <<"testorg">>, proj_name = <<"testproj">>},

    hoax:mock(deliv_scm_local,
              ?expect(process_new_patchset,
                      ?withArgs([Patchset, ProjDir, PipeName, FeatBranchName, RawBranchName, Coords]),
                      ?andReturn({ok, <<"processed\n">>}))),

    hoax:mock(deliv_git,
              ?expect(feature_branch,
                      ?withArgs([PipeName, FeatBranchName]),
                      ?andReturn(SourceBranch))),

    hoax:mock(scm_git_client,
              ?expect(force_push,
                      ?withArgs([SourceBranch, FeatBranchName, Coords]),
                      ?andReturn({error, {git_cmd_failed, caller, path, <<"result">>}}))),

    Result = bitbucket_scm:process_new_patchset(Patchset, ProjDir, PipeName, FeatBranchName, RawBranchName, Coords),

    ?assertEqual({ok, <<"processed\nFailed to push branch to Bitbucket: result">>}, Result),
    ?verifyAll.

process_new_patchset_returns_ok_when_fails_to_push_to_bitbucket_error_git_cmd_error() ->
    Patchset = deliv_patchset:'#new'(),
    ProjDir = <<"testdir">>,
    PipeName = <<"testpipe">>,
    FeatBranchName = <<"testfeatbranch">>,
    RawBranchName = <<"testrawbranch">>,
    SourceBranch = <<"SourceBranch">>,
    Coords = #proj_coordinates{ent_name = <<"testent">>, org_name = <<"testorg">>, proj_name = <<"testproj">>},

    hoax:mock(deliv_scm_local,
              ?expect(process_new_patchset,
                      ?withArgs([Patchset, ProjDir, PipeName, FeatBranchName, RawBranchName, Coords]),
                      ?andReturn({ok, <<"processed\n">>}))),

    hoax:mock(deliv_git,
              ?expect(feature_branch,
                      ?withArgs([PipeName, FeatBranchName]),
                      ?andReturn(SourceBranch))),

    hoax:mock(scm_git_client,
              ?expect(force_push,
                      ?withArgs([SourceBranch, FeatBranchName, Coords]),
                      ?andReturn({error, {git_cmd_error, caller, path, timeout}}))),

    Result = bitbucket_scm:process_new_patchset(Patchset, ProjDir, PipeName, FeatBranchName, RawBranchName, Coords),

    ?assertEqual({ok, <<"processed\nFailed to push branch to Bitbucket: timeout">>}, Result),
    ?verifyAll.

process_new_patchset_returns_ok_when_fails_to_push_to_bitbucket_error_git_error_any() ->
    Patchset = deliv_patchset:'#new'(),
    ProjDir = <<"testdir">>,
    PipeName = <<"testpipe">>,
    FeatBranchName = <<"testfeatbranch">>,
    RawBranchName = <<"testrawbranch">>,
    SourceBranch = <<"SourceBranch">>,
    Coords = #proj_coordinates{ent_name = <<"testent">>, org_name = <<"testorg">>, proj_name = <<"testproj">>},

    hoax:mock(deliv_scm_local,
              ?expect(process_new_patchset,
                      ?withArgs([Patchset, ProjDir, PipeName, FeatBranchName, RawBranchName, Coords]),
                      ?andReturn({ok, <<"processed\n">>}))),

    hoax:mock(deliv_git,
              ?expect(feature_branch,
                      ?withArgs([PipeName, FeatBranchName]),
                      ?andReturn(SourceBranch))),

    hoax:mock(scm_git_client,
              ?expect(force_push,
                      ?withArgs([SourceBranch, FeatBranchName, Coords]),
                      ?andReturn({error, why}))),

    Result = bitbucket_scm:process_new_patchset(Patchset, ProjDir, PipeName, FeatBranchName, RawBranchName, Coords),

    ?assertEqual({ok, <<"processed\nFailed to push branch to Bitbucket">>}, Result),
    ?verifyAll.

pull_request_description_returns_expected_text_test() ->
    ?assertEqual(<<"Bitbucket Pull Request (#42)">>, bitbucket_scm:pull_request_description(42)).

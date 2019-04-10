-module(deliv_scm_github_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_coordinates.hrl").

-compile(export_all).

fixture_test_() ->
    [
     hoax:fixture(?MODULE, clone_url_3),
     hoax:fixture(?MODULE, clone_url_4),
     hoax:fixture(?MODULE, load_config),
     hoax:fixture(?MODULE, patchset_branch),
     hoax:fixture(?MODULE, merge_feature_branch),
     hoax:fixture(?MODULE, delete_feature_branch),
     hoax:fixture(?MODULE, process_new_patchset)
    ].

clone_url_3_returns_git_url() ->
    Patchset = deliv_patchset:setvals([{id, 42}], deliv_patchset:'#new'()),

    hoax:mock(deliv_github_patchset, ?expect(clone_url, ?withArgs([Patchset]), ?andReturn(<<"result">>))),

    Actual = deliv_scm_github:clone_url(ignored, Patchset, ignored),

    ?assertEqual(<<"result">>, Actual),

    ?verifyAll.

clone_url_4_returns_git_url_when_fips_disabled() ->
    ?assertThrow(<<"Github V1 intergration is not supported in FIPS mode. Please upgrade to Github V2 intergration if you wish to run with FIPS enabled on the Automate Server kernal.">>, deliv_scm_github:clone_url(ignored, ignored, ignored, true)),
    ?verifyAll.

clone_url_4_throws_error_when_fips_enabled() ->
    Patchset = deliv_patchset:setvals([{id, 42}], deliv_patchset:'#new'()),

    hoax:mock(deliv_github_patchset, ?expect(clone_url, ?withArgs([Patchset]), ?andReturn(<<"result">>))),

    Actual = deliv_scm_github:clone_url(ignored, Patchset, ignored, false),

    ?assertEqual(<<"result">>, Actual),

    ?verifyAll.

load_config_for_patchset_reads_file_content_from_github_api() ->
    Sha = <<"0d1a26e67d8f5eaf1f6ba5c57fc3c7d91ac0fd1c">>,
    FileContents = <<"{\"version\": \"1\", \"build_cookbook\": \"simple_build\"}">>,
    Patchset = deliv_patchset:setvals([{id, 42}, {sha, Sha}], deliv_patchset:'#new'()),
    Scope = deliv_scopes:'#new_common'([{scoping_names, [<<"EntName">>, <<"OrgName">>, <<"ProjName">>, <<"PipeName">>]},
                                        {proj_id, <<"ProjId">>}]),

    hoax:mock(deliv_github_api,
              ?expect(get_file_contents,
                      ?withArgs([Scope, <<".delivery/config.json">>, Sha]),
                      ?andReturn({ok, FileContents}))),

    ?assertEqual({ok, make_minimal_valid_json()},
                 deliv_scm_github:load_config_for_patchset(Patchset, Scope)),
    ?verifyAll.

load_config_for_patchset_forwards_error_from_file_get() ->
    Sha = <<"0d1a26e67d8f5eaf1f6ba5c57fc3c7d91ac0fd1c">>,
    Patchset = deliv_patchset:setvals([{id, 42}, {sha, Sha}], deliv_patchset:'#new'()),
    Scope = deliv_scopes:'#new_common'([{scoping_names, [<<"EntName">>, <<"OrgName">>, <<"ProjName">>, <<"PipeName">>]},
                                        {proj_id, <<"ProjId">>}]),

    hoax:mock(deliv_github_api,
              ?expect(get_file_contents,
                      ?withArgs([Scope, <<".delivery/config.json">>, Sha]),
                      ?andReturn({error, file_not_found}))),

    ?assertEqual({error, file_not_found},
                 deliv_scm_github:load_config_for_patchset(Patchset, Scope)),
    ?verifyAll.

patchset_branch_returns_branch() ->
    BranchName = <<"HappyBranch">>,
    Change = deliv_change:fromlist([{feature_branch, BranchName}]),
    Patchset = deliv_patchset:'#new'(),
    Scope = deliv_scopes:'#new_common'([]),

    ActualBranch = deliv_scm_github:patchset_branch(Change, Patchset, Scope),

    ?assertEqual(BranchName, ActualBranch).

make_minimal_valid_json() ->
    {[{<<"version">>, <<"1">>},
      {<<"build_cookbook">>, <<"simple_build">>}]}.

merge_feature_branch_returns_sha() ->
    Scope = deliv_scopes:'#new_common'(),
    PatchsetId = 123,
    Sha = <<"6dcb09b5b57875f334f61aebed695e2e4193db5e">>,
    ChangeId = <<"456">>,
    Patchset = deliv_patchset:'#new'(),
    Patchset1 = deliv_patchset:setvals([{id, PatchsetId}, {change_id, ChangeId}, {sha, Sha}], Patchset),
    Approver = deliv_user:'#new'(),
    Approver1 = deliv_user:setvals([{name, "baxterthehacker"}, {email, "baxter@thehacker.com"}], Approver),

    CommitMsg = erlang:iolist_to_binary(["Merged change ", ChangeId, "\n\n",
                                         "From review branch ", "changes", " into ", "master", "\n\n",
                                         "Signed-off-by: ", ["baxterthehacker", " <", "baxter@thehacker.com", ">"]]),

    Json = chef_json:encode(ct_github:load_payload_from_disk(<<"pull_request_opened.json">>)),
    GithubPatchset = deliv_github_patchset:'#new'(),

    hoax:mock(deliv_github_patchset,
              [?expect(fetch_by_patchset_id,
                       ?withArgs([PatchsetId]),
                       ?andReturn([GithubPatchset])),
               ?expect(getval,
                       ?withArgs([payload, GithubPatchset]),
                       ?andReturn(Json))]),
    hoax:mock(deliv_github_api,
              ?expect(merge_pull_request,
                      ?withArgs([Scope, 1, Sha, CommitMsg]),
                      ?andReturn({ok, Sha}))),

    Response = deliv_scm_github:merge_feature_branch(Scope, Patchset1, Approver1),
    ?assertEqual({ok, Sha}, Response),
    ?verifyAll.

merge_feature_branch_errors_on_invalid_patchset() ->
    Scope = deliv_scopes:'#new_common'(),
    PatchsetId = 123,
    Patchset = deliv_patchset:'#new'(),
    Patchset1 = deliv_patchset:setvals([{id, PatchsetId}], Patchset),
    Approver = deliv_user:'#new'(),

    hoax:mock(deliv_github_patchset,
              ?expect(fetch_by_patchset_id,
                      ?withArgs([PatchsetId]),
                      ?andReturn([]))),
    Response = deliv_scm_github:merge_feature_branch(Scope, Patchset1, Approver),
    ?assertEqual({error, github_patchset_not_found}, Response),
    ?verifyAll.

delete_feature_branch_returns_ok_with_branch_on_success() ->
    Scope = deliv_scopes:'#new_common'(),
    PatchsetId = 123,
    Patchset = deliv_patchset:'#new'(),
    Patchset1 = deliv_patchset:setvals([{id, PatchsetId}], Patchset),

    Json = chef_json:encode(ct_github:load_payload_from_disk(<<"pull_request_opened.json">>)),
    GithubPatchset = deliv_github_patchset:'#new'(),

    hoax:mock(deliv_github_patchset,
              [?expect(fetch_by_patchset_id,
                       ?withArgs([PatchsetId]),
                       ?andReturn([GithubPatchset])),
               ?expect(getval,
                       ?withArgs([payload, GithubPatchset]),
                       ?andReturn(Json))]),
    hoax:mock(deliv_github_api,
              ?expect(delete_branch,
                      ?withArgs([Scope, <<"changes">>]),
                      ?andReturn(ok))),
    Response = deliv_scm_github:delete_feature_branch(Scope, Patchset1),
    ?assertEqual({ok, <<"changes">>}, Response),
    ?verifyAll.

delete_feature_branch_returns_error_on_failure() ->
    Scope = deliv_scopes:'#new_common'(),
    PatchsetId = 123,
    Patchset = deliv_patchset:'#new'(),
    Patchset1 = deliv_patchset:setvals([{id, PatchsetId}], Patchset),

    Json = chef_json:encode(ct_github:load_payload_from_disk(<<"pull_request_opened.json">>)),
    GithubPatchset = deliv_github_patchset:'#new'(),

    hoax:mock(deliv_github_patchset,
              [?expect(fetch_by_patchset_id,
                       ?withArgs([PatchsetId]),
                       ?andReturn([GithubPatchset])),
               ?expect(getval,
                       ?withArgs([payload, GithubPatchset]),
                       ?andReturn(Json))]),
    hoax:mock(deliv_github_api,
              ?expect(delete_branch,
                      ?withArgs([Scope, <<"changes">>]),
                      ?andReturn({error, github_branch_delete_failed}))),
    Response = deliv_scm_github:delete_feature_branch(Scope, Patchset1),
    ?assertEqual({error, feature_branch_delete_failed}, Response),
    ?verifyAll.

process_new_patchset_not_implemented() ->
    PatchSet = deliv_patchset:'#new'(),
    Response = deliv_scm_github:process_new_patchset(PatchSet, <<"PDir">>,
                                                     <<"PName">>, <<"FB">>,
                                                     <<"RB">>, #proj_coordinates{}),
    ?assertEqual({error, <<"not implemented">>}, Response).

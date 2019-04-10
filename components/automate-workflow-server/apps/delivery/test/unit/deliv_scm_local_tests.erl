-module(deliv_scm_local_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_coordinates.hrl").

-compile(export_all).

fixture_test_() ->
    [
     hoax:fixture(?MODULE, clone_url_3),
     hoax:fixture(?MODULE, clone_url_4),
     hoax:fixture(?MODULE, load_config),
     hoax:fixture(?MODULE, patchset_branch),
     hoax:fixture(?MODULE, delete_feature_branch),
     hoax:fixture(?MODULE, merge_feature_branch),
     hoax:fixture(?MODULE, process_new_patchset)
    ].

clone_url_3_returns_git_url() ->
    hoax:mock(delivery_app, [
                             ?expect(get_env,
                                     ?withArgs([deliv_ssh_git_hostname]),
                                     ?andReturn(<<"delivery-server">>)),
                             ?expect(get_env,
                                     ?withArgs([deliv_ssh_git_port]),
                                     ?andReturn(8989))
                            ]),
    Patchset = deliv_patchset:'#new'(),
    Scope = deliv_scopes:'#new_common'([{scoping_names, [<<"EntName">>, <<"OrgName">>, <<"ProjName">>, <<"PipeName">>]}]),
    ?assertEqual(<<"ssh://builder@EntName@delivery-server:8989/EntName/OrgName/ProjName">>,
                 deliv_scm_local:clone_url(<<"builder">>, Patchset, Scope)),

    ?verifyAll.

clone_url_4_returns_git_url_when_fips_is_false() ->
    hoax:mock(delivery_app, [
                             ?expect(get_env,
                                     ?withArgs([deliv_ssh_git_hostname]),
                                     ?andReturn(<<"delivery-server">>)),
                             ?expect(get_env,
                                     ?withArgs([deliv_ssh_git_port]),
                                     ?andReturn(8989))
                            ]),
    Patchset = deliv_patchset:'#new'(),
    Scope = deliv_scopes:'#new_common'([{scoping_names, [<<"EntName">>, <<"OrgName">>, <<"ProjName">>, <<"PipeName">>]}]),
    ?assertEqual(<<"ssh://builder@EntName@delivery-server:8989/EntName/OrgName/ProjName">>,
                 deliv_scm_local:clone_url(<<"builder">>, Patchset, Scope, false)),

    ?verifyAll.

clone_url_4_returns_git_fips_url_when_fips_is_true() ->
    hoax:mock(delivery_app, [
                             ?expect(get_env,
                                     ?withArgs([deliv_fips_stunnel_proxy_port]),
                                     ?andReturn(<<"11111">>))
                            ]),
    Patchset = deliv_patchset:'#new'(),
    Scope = deliv_scopes:'#new_common'([{scoping_names, [<<"EntName">>, <<"OrgName">>, <<"ProjName">>, <<"PipeName">>]}]),
    ?assertEqual(<<"ssh://builder@EntName@localhost:11111/EntName/OrgName/ProjName">>,
                 deliv_scm_local:clone_url(<<"builder">>, Patchset, Scope, true)),

    ?verifyAll.

load_config_for_patchset_reads_file_content_from_git() ->
    Patchset = deliv_patchset:setvals([{sha, <<"SHA">>}], deliv_patchset:'#new'()),
    Scope = deliv_scopes:'#new_common'([{scoping_names, [<<"EntName">>, <<"OrgName">>, <<"ProjName">>, <<"PipeName">>]}]),

    ProjectConfig = make_minimal_valid_json(),
    hoax:mock(deliv_project,
              ?expect(repo_path,
                      ?withArgs([<<"EntName">>, <<"OrgName">>, <<"ProjName">>]),
                      ?andReturn({ok, <<"repo path">>}))),
    hoax:mock(deliv_git,
              ?expect(file_at_sha,
                      ?withArgs([<<"repo path">>, <<"SHA">>, <<".delivery/config.json">>]),
                      ?andReturn({ok, ProjectConfig}))),

    Actual = deliv_scm_local:load_config_for_patchset(Patchset, Scope),

    ?assertEqual({ok, ProjectConfig}, Actual),
    ?verifyAll.

patchset_branch_composes_branch_name() ->
    PipelineName = <<"fakePipeline">>,
    FeatBranchName = <<"fakeFeatureBranch">>,
    SequenceNumber = 1,
    hoax:mock(deliv_git,
              ?expect(patchset_branch,
                      ?withArgs([PipelineName, FeatBranchName, SequenceNumber]),
                      ?andReturn("fakeBranch"))),

    Change = deliv_change:fromlist([{feature_branch, FeatBranchName}]),
    Patchset = deliv_patchset:fromlist([{sequence_number, SequenceNumber }]),
    Scope = deliv_scopes:'#new_common'([{pipe_name, PipelineName}]),

    Branch = deliv_scm_local:patchset_branch(Change, Patchset, Scope),

    ?assertEqual(<<"fakeBranch">>, Branch).

delete_feature_branch_deletes_local_git_branch_upon_success() ->
    ChangeId = <<"mockedChangeGuid">>,
    ProjectId = <<"projectId">>,
    PipeName = <<"master">>,
    SeqNum = 1,
    FeatureBranch = <<"my_change">>,

    Scope = deliv_scopes:'#new_common'([{change_id, ChangeId},
                                        {proj_id, ProjectId},
                                        {pipe_name, PipeName}]),
    Patchset0 = deliv_patchset:'#new'(),
    Patchset = deliv_patchset:setvals([{sequence_number, SeqNum}], Patchset0),

    Change0 = deliv_change:'#new'(),
    Change = deliv_change:setvals([{feature_branch, FeatureBranch}], Change0),

    Project = deliv_project:'#new'(),

    ProjDir = <<"git/repo/on/disk">>,
    PatchsetBranch = <<"_reviews/master/my_change/1">>,
    BranchesBin = <<"refs/heads/_reviews/master/my_change/1\nrefs/heads/_reviews/master/my_change/latest">>,
    ReviewPrefix = <<"refs/heads/_reviews/master/my_change">>,
    DelCmd = ["branch", "-D"] ++ [<<"refs/heads/_reviews/master/my_change/1">>, <<"refs/heads/_reviews/master/my_change/latest">>],

    hoax:mock(deliv_db, [
                         ?expect(fetch_by_id,
                                 ?withArgs([deliv_change, ChangeId]),
                                 ?andReturn({ok, Change}))
                        ]),
    hoax:mock(deliv_project, [
                              ?expect(fetch_by_id,
                                      ?withArgs([ProjectId]),
                                      ?andReturn({ok, Project})),
                              ?expect(repo_path,
                                      ?withArgs([Project]),
                                      ?andReturn(ProjDir))
                             ]),
    hoax:mock(deliv_git, [
                          ?expect(run_git,
                                  ?withArgs([delete_review_branch, ProjDir,
                                             ["for-each-ref",
                                              "--format=%(refname:short)",
                                              ReviewPrefix]]),
                                  ?andReturn({ok, BranchesBin})),
                          ?expect(run_git,
                                  ?withArgs([delete_review_branch2, ProjDir, DelCmd]),
                                  ?andReturn({ok, ignored})),
                          ?expect(patchset_branch,
                                  ?withArgs([PipeName, FeatureBranch, SeqNum]),
                                  ?andReturn(PatchsetBranch))
                         ]),

    Actual = deliv_scm_local:delete_feature_branch(Scope, Patchset),
    ?assertEqual({ok, ignored}, Actual),
    ?verifyAll.

merge_feature_branch_returns_merge_sha_upon_success() ->
    ChangeId = <<"mockedChangeGuid">>,
    ProjectId = <<"projectId">>,
    PipeName = <<"master">>,
    SeqNum = 1,
    FeatureBranch = <<"my_change">>,
    Pid = pid,

    Scope = deliv_scopes:'#new_common'([{change_id, ChangeId},
                                        {proj_id, ProjectId},
                                        {pipe_name, PipeName}]),
    Patchset0 = deliv_patchset:'#new'(),
    Patchset = deliv_patchset:setvals([{sequence_number, SeqNum}], Patchset0),

    Change0 = deliv_change:'#new'(),
    Change = deliv_change:setvals([{feature_branch, FeatureBranch}], Change0),

    Project = deliv_project:'#new'(),
    Approver = deliv_user:'#new'(),

    ProjDir = <<"git/repo/on/disk">>,
    PatchsetBranch = <<"_reviews/master/my_change/1">>,
    MergeSha = <<"merge_sha">>,

    hoax:mock(deliv_db, [
                         ?expect(fetch_by_id,
                                 ?withArgs([deliv_change, ChangeId]),
                                 ?andReturn({ok, Change}))
                        ]),
    hoax:mock(deliv_project, [
                              ?expect(fetch_by_id,
                                      ?withArgs([ProjectId]),
                                      ?andReturn({ok, Project})),
                              ?expect(repo_path,
                                      ?withArgs([Project]),
                                      ?andReturn(ProjDir))
                             ]),
    hoax:mock(deliv_git, [
                          ?expect(branch_exists,
                                  ?withArgs([ProjDir, PipeName]),
                                  ?andReturn(branch_exists)),
                          ?expect(patchset_branch,
                                  ?withArgs([PipeName, FeatureBranch, SeqNum]),
                                  ?andReturn(PatchsetBranch))
                         ]),
    hoax:mock(deliv_git_working_tree_sup,
              ?expect(get_worker,
                      ?withArgs([Project]),
                      ?andReturn({ok, Pid}))),
    hoax:mock(deliv_git_working_tree,
              ?expect(merge_change,
                      ?withArgs([Pid, PatchsetBranch, PipeName, ChangeId, Approver]),
                      ?andReturn({ok, MergeSha}))),

    Actual = deliv_scm_local:merge_feature_branch(Scope, Patchset, Approver),
    ?assertEqual({ok, MergeSha}, Actual),
    ?verifyAll.

merge_feature_branch_returns_error_on_merge_failure() ->
    ChangeId = <<"mockedChangeGuid">>,
    ProjectId = <<"projectId">>,
    PipeName = <<"master">>,
    SeqNum = 1,
    FeatureBranch = <<"my_change">>,
    Pid = pid,

    Scope = deliv_scopes:'#new_common'([{change_id, ChangeId},
                                        {proj_id, ProjectId},
                                        {pipe_name, PipeName}]),
    Patchset = deliv_patchset:fromlist([{sequence_number, SeqNum}]),

    Change = deliv_change:fromlist([{feature_branch, FeatureBranch}]),

    Project = deliv_project:'#new'(),
    Approver = deliv_user:'#new'(),

    ProjDir = <<"git/repo/on/disk">>,
    PatchsetBranch = <<"_reviews/master/my_change/1">>,

    hoax:mock(deliv_db, [
                         ?expect(fetch_by_id,
                                 ?withArgs([deliv_change, ChangeId]),
                                 ?andReturn({ok, Change}))
                        ]),
    hoax:mock(deliv_project, [
                              ?expect(fetch_by_id,
                                      ?withArgs([ProjectId]),
                                      ?andReturn({ok, Project})),
                              ?expect(repo_path,
                                      ?withArgs([Project]),
                                      ?andReturn(ProjDir))
                             ]),
    hoax:mock(deliv_git, [
                          ?expect(branch_exists,
                                  ?withArgs([ProjDir, PipeName]),
                                  ?andReturn(branch_exists)),
                          ?expect(patchset_branch,
                                  ?withArgs([PipeName, FeatureBranch, SeqNum]),
                                  ?andReturn(PatchsetBranch))
                         ]),
    hoax:mock(deliv_git_working_tree_sup,
              ?expect(get_worker,
                      ?withArgs([Project]),
                      ?andReturn({ok, Pid}))),
    hoax:mock(deliv_git_working_tree,
              ?expect(merge_change,
                      ?withArgs([Pid, PatchsetBranch, PipeName, ChangeId, Approver]),
                      ?andReturn({error, feature_branch_merge_failed}))),

    Actual = deliv_scm_local:merge_feature_branch(Scope, Patchset, Approver),
    ?assertEqual({error, feature_branch_merge_failed}, Actual),
    ?verifyAll.

process_new_patchset_creates_git_branches_and_sets_change_title_desc_when_undef() ->
    Ent = <<"Ent">>,
    Org = <<"Org">>,
    Proj = <<"Proj">>,
    ProjectCoords = #proj_coordinates{ent_name = Ent,
                                      org_name = Org,
                                      proj_name = Proj},
    PatchsetNumber = 1,
    PSNumBin = chef_utils:to_bin(PatchsetNumber),
    ChangeId = <<"changeid">>,
    ProjectDir = <<"PDir">>,
    PipeName = <<"pipe_name">>,
    FeatBranchName = <<"feat1">>,
    RawBranchName = <<"_for/", PipeName/binary, "/", FeatBranchName/binary>>,
    ReviewBranch = <<"_reviews/", PipeName/binary, "/", FeatBranchName/binary, "/", PSNumBin/binary>>,
    FeatureBranch = <<"_reviews/", PipeName/binary, "/", FeatBranchName/binary, "/latest">>,
    CommitTitle = <<"title">>,
    CommitDesc = <<"desc">>,

    PatchSet0 = deliv_patchset:'#new'(),
    PatchSet1 = deliv_patchset:setvals([{sequence_number, PatchsetNumber},
                                        {change_id, ChangeId}], PatchSet0),

    Change0 = deliv_change:'#new'(),
    Change1 = deliv_change:setvals([{id, ChangeId}], Change0),
    Change2 = deliv_change:setvals([{title, CommitTitle}], Change1),
    Change3 = deliv_change:setvals([{description, CommitDesc}], Change2),

    hoax:mock(deliv_git,
              ?expect(run_git,
                      ?withArgs([handle_new_patchset, ProjectDir,
                                 ["branch", "--move", RawBranchName, ReviewBranch]]),
                      ?andReturn({ok, <<"output">>}))),
    hoax:mock(deliv_git,
              ?expect(run_git,
                      ?withArgs([handle_new_patchset, ProjectDir,
                                 ["branch", "--force", FeatureBranch, ReviewBranch]]),
                      ?andReturn({ok, <<"output">>}))),
    hoax:mock(deliv_git,
              ?expect(run_git,
                      ?withArgs([handle_new_patchset, ProjectDir,
                                 ["log", "--format=%B", "-1", ReviewBranch]]),
                      ?andReturn({ok, <<CommitTitle/binary, "\n\n", CommitDesc/binary>>}))),
    hoax:mock(deliv_git,
              ?expect(patchset_branch,
                      ?withArgs([PipeName, FeatBranchName, PatchsetNumber]),
                      ?andReturn(ReviewBranch))),
    hoax:mock(deliv_git,
              ?expect(feature_branch,
                      ?withArgs([PipeName, FeatBranchName]),
                      ?andReturn(FeatureBranch))),

    hoax:mock(deliv_change,
              ?expect(fetch_by_id,
                      ?withArgs([ChangeId]),
                      ?andReturn({ok, Change1}))),
    hoax:mock(deliv_change,
              ?expect(update,
                      ?withArgs([Change3]),
                      ?andReturn({ok, Change3}))),
    hoax:mock(deliv_change,
              ?expect(verify_patchset,
                      ?withArgs([ChangeId]),
                      ?andReturn(ok))),
    hoax:mock(deliv_change,
              ?expect(getval,
                      ?withArgs([title, Change1]),
                      ?andReturn(undefined))),
    hoax:mock(deliv_change,
              ?expect(getval,
                      ?withArgs([description, Change2]),
                      ?andReturn(undefined))),
    hoax:mock(deliv_change,
              ?expect(setvals,
                      ?withArgs([[{title, CommitTitle}], Change1]),
                      ?andReturn(Change2))),
    hoax:mock(deliv_change,
              ?expect(setvals,
                      ?withArgs([[{description, CommitDesc}], Change2]),
                      ?andReturn(Change3))),

    Url = <<"url">>,
    hoax:mock(deliv_web_utils,
              ?expect(make_web_url_for_change,
                      ?withArgs([Ent, Org, Proj, ChangeId]),
                      ?andReturn(Url))),

    Response = deliv_scm_local:process_new_patchset(PatchSet1, ProjectDir,
                                                    PipeName, FeatBranchName,
                                                    RawBranchName, ProjectCoords),
    ?assertEqual({ok, <<"Created new patchset\n", Url/binary, "\n">>}, Response),
    ?verifyAll.

process_new_patchset_creates_git_branches() ->
    Ent = <<"Ent">>,
    Org = <<"Org">>,
    Proj = <<"Proj">>,
    ProjectCoords = #proj_coordinates{ent_name = Ent,
                                      org_name = Org,
                                      proj_name = Proj},
    PatchsetNumber = 1,
    PSNumBin = chef_utils:to_bin(PatchsetNumber),
    ChangeId = <<"changeid">>,
    ProjectDir = <<"PDir">>,
    PipeName = <<"pipe_name">>,
    FeatBranchName = <<"feat1">>,
    RawBranchName = <<"_for/", PipeName/binary, "/", FeatBranchName/binary>>,
    ReviewBranch = <<"_reviews/", PipeName/binary, "/", FeatBranchName/binary, "/", PSNumBin/binary>>,
    FeatureBranch = <<"_reviews/", PipeName/binary, "/", FeatBranchName/binary, "/latest">>,
    CommitTitle = <<"title">>,
    CommitDesc = <<"desc">>,

    PatchSet0 = deliv_patchset:'#new'(),
    PatchSet1 = deliv_patchset:setvals([{sequence_number, PatchsetNumber},
                                        {change_id, ChangeId}], PatchSet0),

    Change0 = deliv_change:'#new'(),
    Change1 = deliv_change:setvals([{id, ChangeId},
                                    {title, CommitTitle},
                                    {description, CommitDesc}], Change0),

    hoax:mock(deliv_git,
              ?expect(run_git,
                      ?withArgs([handle_new_patchset, ProjectDir,
                                 ["branch", "--move", RawBranchName, ReviewBranch]]),
                      ?andReturn({ok, <<"output">>}))),
    hoax:mock(deliv_git,
              ?expect(run_git,
                      ?withArgs([handle_new_patchset, ProjectDir,
                                 ["branch", "--force", FeatureBranch, ReviewBranch]]),
                      ?andReturn({ok, <<"output">>}))),
    hoax:mock(deliv_git,
              ?expect(run_git,
                      ?withArgs([handle_new_patchset, ProjectDir,
                                 ["log", "--format=%B", "-1", ReviewBranch]]),
                      ?andReturn({ok, <<CommitTitle/binary, "\n\n", CommitDesc/binary>>}))),
    hoax:mock(deliv_git,
              ?expect(patchset_branch,
                      ?withArgs([PipeName, FeatBranchName, PatchsetNumber]),
                      ?andReturn(ReviewBranch))),
    hoax:mock(deliv_git,
              ?expect(feature_branch,
                      ?withArgs([PipeName, FeatBranchName]),
                      ?andReturn(FeatureBranch))),

    hoax:mock(deliv_change,
              ?expect(fetch_by_id,
                      ?withArgs([ChangeId]),
                      ?andReturn({ok, Change1}))),
    hoax:mock(deliv_change,
              ?expect(update,
                      ?withArgs([Change1]),
                      ?andReturn({ok, Change1}))),
    hoax:mock(deliv_change,
              ?expect(verify_patchset,
                      ?withArgs([ChangeId]),
                      ?andReturn(ok))),
    hoax:mock(deliv_change,
              ?expect(getval,
                      ?withArgs([title, Change1]),
                      ?andReturn(CommitTitle))),
    hoax:mock(deliv_change,
              ?expect(getval,
                      ?withArgs([description, Change1]),
                      ?andReturn(CommitDesc))),

    Url = <<"url">>,
    hoax:mock(deliv_web_utils,
              ?expect(make_web_url_for_change,
                      ?withArgs([Ent, Org, Proj, ChangeId]),
                      ?andReturn(Url))),

    Response = deliv_scm_local:process_new_patchset(PatchSet1, ProjectDir,
                                                    PipeName, FeatBranchName,
                                                    RawBranchName, ProjectCoords),
    ?assertEqual({ok, <<"Created new patchset\n", Url/binary, "\n">>}, Response),
    ?verifyAll.

make_minimal_valid_json() ->
    {[{<<"version">>, <<"1">>},
      {<<"build_cookbook">>, <<"simple_build">>}]}.

%% Copyright 2014 Opscode, Inc. All Rights Reserved.

-module(deliv_project_tests).

-include_lib("hoax/include/hoax.hrl").

-compile([export_all]).


deliv_project_fixtures_test_() ->
    [
        hoax:fixture(?MODULE, projects),
        hoax:fixture(?MODULE, scm),
        hoax:fixture(?MODULE, new),
        hoax:fixture(?MODULE, convert_to_bb),
        hoax:fixture(?MODULE, convert_to_local),
        hoax:fixture(?MODULE, convert_ghv2_to_local),
        hoax:fixture(?MODULE, convert_to_githubV2),
        hoax:fixture(?MODULE, metadata_module),
        hoax:fixture(?MODULE, delete)
    ].

decompose_repo_name_test_() ->
    [
     {"Basic functionality",
      ?_assertEqual(
         {<<"ent name">>, <<"org/name">>, <<"proj@">>},
         deliv_project:decompose_repo_name("ent%20name/org%2Fname/proj%40")
        )},
     {"Error on invalid input",
      ?_assertEqual(
         error,
         deliv_project:decompose_repo_name("ent/org/proj/blah")
        )}
    ].

compose_repo_name_test_() ->
    [
     {"Basic functionality",
      ?_assertEqual(
         <<"ent%20name/org%2Fname/proj%40">>,
         deliv_project:compose_repo_name(<<"ent name">>, <<"org/name">>, <<"proj@">>)
        )},
     {"Round trip",
      ?_assertEqual(
         {<<"ent name">>, <<"org/name">>, <<"proj@">>},
         deliv_project:decompose_repo_name(
           deliv_project:compose_repo_name(<<"ent name">>, <<"org/name">>, <<"proj@">>))
        )}
    ].

delete_returns_the_number_of_projects_deleted() ->
    EntName = <<"Ent">>,
    OrgName = <<"Org">>,
    ProjName = <<"Proj">>,
    Proj0 = deliv_project:'#new'(),
    Proj = deliv_project:setvals([{guid, <<"7f695370-0eeb-11e4-9191-0800200c9a66">>}], Proj0),

    hoax:mock(deliv_git,
        ?expect(delete_repo,
            ?withArgs([<<"/tmp/test_git_repos/7f/69/53/70/0eeb-11e4-9191-0800200c9a66">>]),
            ?andReturn(ok))),
    hoax:mock(deliv_db, [
        ?expect(fetch,
            ?withArgs([deliv_project, [EntName, OrgName], ProjName]),
            ?andReturn({ok, Proj})),
        ?expect(delete_unique_by_scoping_params,
            ?withArgs([deliv_project, [EntName, OrgName], ProjName]),
            ?andReturn({ok, 1}))
    ]),
    hoax:mock(delivery_app,
        ?expect(get_env,
            ?withArgs([deliv_git_repos_root]),
            ?andReturn(<<"/tmp/test_git_repos">>))),

    ?assertEqual({ok, 1}, deliv_project:delete(EntName, OrgName, ProjName, true)),
    ?verifyAll.

projects_returns_list_of_projects() ->
    EntName = <<"Ent">>,
    OrgId = 1,
    OrgName = <<"Org">>,
    Org = deliv_organization:fromlist([{id, OrgId}, {name, OrgName}]),
    Projects = [project1, project2],

    hoax:mock(deliv_organization, [
        ?expect(fetch,
            ?withArgs([EntName, OrgName]),
            ?andReturn({ok, Org})),
        ?expect(getval,
            ?withArgs([id, Org]),
            ?andReturn(OrgId))
    ]),
    hoax:mock(deliv_db,
        ?expect(qfetch,
            ?withArgs([deliv_project, projects, [OrgId]]),
            ?andReturn(Projects))),

    ?assertEqual(Projects, deliv_project:projects(EntName, OrgName)).

scm_type_returns_the_projects_scm_provider() ->
    LocalProj = deliv_project:fromlist([{scm_module, <<"deliv_scm_local">>}]),
    GitHubV1Proj = deliv_project:fromlist([{scm_module, <<"deliv_scm_github">>}]),
    GitHubV2Proj = deliv_project:fromlist([{scm_module, <<"github_scm">>}]),
    BitbucketProj = deliv_project:fromlist([{scm_module, <<"bitbucket_scm">>}]),

    ?assertEqual(<<"local">>, deliv_project:scm_type(LocalProj)),
    ?assertEqual(<<"github">>, deliv_project:scm_type(GitHubV1Proj)),
    ?assertEqual(<<"githubV2">>, deliv_project:scm_type(GitHubV2Proj)),
    ?assertEqual(<<"bitbucket">>, deliv_project:scm_type(BitbucketProj)).

new_arity7_with_deliv_github_scm_calls_out_to_create_scm_project_with_the_old_gh_type() ->
    EntName = <<"Enterprise">>,
    OrgName = <<"Org">>,
    ProjName = <<"Project">>,
    PipeName = <<"master">>,
    ScmModule = deliv_scm_github,
    RepoOwner = <<"scm_org">>,
    RepoName = <<"scm_repo">>,
    Params = [EntName, OrgName, ProjName, PipeName, ScmModule, RepoOwner, RepoName],
    Proj = deliv_project:'#new'(),

    hoax:mock(deliv_db,
              ?expect(qfetch,
                      ?withArgs([deliv_project, create_scm_project, Params]),
                      ?andReturn([Proj]))),

    ?assertEqual({ok, Proj}, deliv_project:new(EntName, OrgName, ProjName, PipeName, ScmModule, RepoOwner, RepoName)),
    ?verifyAll.

new_arity7_with_bitbucket_scm_calls_out_to_create_bitbucket_scm_project_function() ->
    EntName = <<"Enterprise">>,
    OrgName = <<"Org">>,
    ProjName = <<"Project">>,
    PipeName = <<"master">>,
    ScmModule = bitbucket_scm,
    BitbucketProject = <<"bitbucket_proj">>,
    RepoName = <<"scm_repo">>,
    Params = [EntName, OrgName, ProjName, PipeName, ScmModule, BitbucketProject, RepoName],
    Proj0 = deliv_project:'#new'(),
    Proj1 = deliv_project:setvals([{guid, <<"7f695370-0eeb-11e4-9191-0800200c9a66">>}], Proj0),

    hoax:mock(deliv_db,
              ?expect(qfetch,
                      ?withArgs([deliv_project, create_bitbucket_scm_project, Params]),
                      ?andReturn([Proj1]))),
    hoax:mock(delivery_app,
              ?expect(get_env,
                      ?withArgs([deliv_git_repos_root]),
                      ?andReturn(<<"repo_root">>))),
    hoax:mock(deliv_git,
              ?expect(create_repo,
                      ?withArgs([<<"repo_root/7f/69/53/70/0eeb-11e4-9191-0800200c9a66">>]),
                      ?andReturn({ok, <<"output">>}))),
    hoax:mock(deliv_git,
              ?expect(ensure_hooks,
                      ?withArgs([<<"repo_root/7f/69/53/70/0eeb-11e4-9191-0800200c9a66">>]),
                      ?andReturn(ok))),

    ?assertEqual({ok, Proj1}, deliv_project:new(EntName, OrgName, ProjName, PipeName, ScmModule, BitbucketProject, RepoName)),
    ?verifyAll.

new_arity7_with_github_scm_returns_an_error_when_create_scm_project_fails() ->
    EntName = <<"Enterprise">>,
    OrgName = <<"Org">>,
    ProjName = <<"Project">>,
    PipeName = <<"master">>,
    ScmModule = github_scm,
    RepoOwner = <<"github_proj">>,
    RepoName = <<"scm_repo">>,
    Error = {error, whyyy},
    Params = [EntName, OrgName, ProjName, PipeName, ScmModule, RepoOwner, RepoName],

    hoax:mock(deliv_db,
              ?expect(qfetch,
                      ?withArgs([deliv_project, create_scm_project, Params]),
                      ?andReturn(Error))),

    ?assertEqual(Error, deliv_project:new(EntName, OrgName, ProjName, PipeName, ScmModule, RepoOwner, RepoName)),
    ?verifyAll.

new_arity7_with_github_scm_deletes_project_and_returns_an_error_when_create_repo_fails() ->
    EntName = <<"Enterprise">>,
    OrgName = <<"Org">>,
    ProjName = <<"Project">>,
    PipeName = <<"master">>,
    ScmModule = github_scm,
    RepoOwner = <<"github_proj">>,
    RepoName = <<"scm_repo">>,
    Params = [EntName, OrgName, ProjName, PipeName, ScmModule, RepoOwner, RepoName],
    Proj0 = deliv_project:'#new'(),
    Proj1 = deliv_project:setvals([{guid, <<"7f695370-0eeb-11e4-9191-0800200c9a66">>}], Proj0),
    Error = {error, whyyyy},

    hoax:mock(deliv_db, [
              ?expect(qfetch,
                      ?withArgs([deliv_project, create_scm_project, Params]),
                      ?andReturn([Proj1])),
              ?expect(delete,
                      ?withArgs([Proj1]),
                      ?andReturn(ok))
                      ]),
    hoax:mock(delivery_app,
              ?expect(get_env,
                      ?withArgs([deliv_git_repos_root]),
                      ?andReturn(<<"repo_root">>))),
    hoax:mock(deliv_git,
              ?expect(create_repo,
                      ?withArgs([<<"repo_root/7f/69/53/70/0eeb-11e4-9191-0800200c9a66">>]),
                      ?andReturn(Error))),

    ?assertEqual(Error, deliv_project:new(EntName, OrgName, ProjName, PipeName, ScmModule, RepoOwner, RepoName)),
    ?verifyAll.

new_arity7_with_github_scm_and_returns_error_when_ensure_hooks_fails() ->
    EntName = <<"Enterprise">>,
    OrgName = <<"Org">>,
    ProjName = <<"Project">>,
    PipeName = <<"master">>,
    ScmModule = github_scm,
    RepoOwner = <<"github_proj">>,
    RepoName = <<"scm_repo">>,
    Params = [EntName, OrgName, ProjName, PipeName, ScmModule, RepoOwner, RepoName],
    Proj0 = deliv_project:'#new'(),
    Proj1 = deliv_project:setvals([{guid, <<"7f695370-0eeb-11e4-9191-0800200c9a66">>}], Proj0),
    Error = {error, whyyyy},

    hoax:mock(deliv_db,
              ?expect(qfetch,
                      ?withArgs([deliv_project, create_scm_project, Params]),
                      ?andReturn([Proj1]))),
    hoax:mock(delivery_app,
              ?expect(get_env,
                      ?withArgs([deliv_git_repos_root]),
                      ?andReturn(<<"repo_root">>))),
    hoax:mock(deliv_git,
              ?expect(create_repo,
                      ?withArgs([<<"repo_root/7f/69/53/70/0eeb-11e4-9191-0800200c9a66">>]),
                      ?andReturn({ok, <<"output">>}))),
    hoax:mock(deliv_git,
              ?expect(ensure_hooks,
                      ?withArgs([<<"repo_root/7f/69/53/70/0eeb-11e4-9191-0800200c9a66">>]),
                      ?andReturn(Error))),

    ?assertEqual(Error, deliv_project:new(EntName, OrgName, ProjName, PipeName, ScmModule, RepoOwner, RepoName)),
    ?verifyAll.

new_arity7_with_github_scm_calls_out_to_create_scm_project_with_new_gh_type() ->
    EntName = <<"Enterprise">>,
    OrgName = <<"Org">>,
    ProjName = <<"Project">>,
    PipeName = <<"master">>,
    ScmModule = github_scm,
    RepoOwner = <<"github_proj">>,
    RepoName = <<"scm_repo">>,
    Params = [EntName, OrgName, ProjName, PipeName, ScmModule, RepoOwner, RepoName],
    Proj0 = deliv_project:'#new'(),
    Proj1 = deliv_project:setvals([{guid, <<"7f695370-0eeb-11e4-9191-0800200c9a66">>}], Proj0),

    hoax:mock(deliv_db,
              ?expect(qfetch,
                      ?withArgs([deliv_project, create_scm_project, Params]),
                      ?andReturn([Proj1]))),
    hoax:mock(delivery_app,
              ?expect(get_env,
                      ?withArgs([deliv_git_repos_root]),
                      ?andReturn(<<"repo_root">>))),
    hoax:mock(deliv_git, [
              ?expect(create_repo,
                      ?withArgs([<<"repo_root/7f/69/53/70/0eeb-11e4-9191-0800200c9a66">>]),
                      ?andReturn({ok, <<"output">>})),
              ?expect(ensure_hooks,
                      ?withArgs([<<"repo_root/7f/69/53/70/0eeb-11e4-9191-0800200c9a66">>]),
                      ?andReturn(ok))
                      ]),

    ?assertEqual({ok, Proj1}, deliv_project:new(EntName, OrgName, ProjName, PipeName, ScmModule, RepoOwner, RepoName)),
    ?verifyAll.

convert_to_bb_when_qfetch_fails_returns_an_error() ->
    ProjId = <<"ProjId">>,
    Proj = deliv_project:fromlist([{id, ProjId}]),
    BitbucketKey = <<"DEMO">>,
    BitbucketRepo = <<"demo">>,
    Json = {[{<<"scm">>,
                {[{<<"type">>,<<"bitbucket">>},
                  {<<"projectCreateUri">>,
                   <<"/api/v0/e/cd/orgs/sandbox/bitbucket-projects">>},
                  {<<"scmSetupConfigs">>,
                   [{[{<<"root_api_url">>,
                       <<"https://10.194.11.251">>},
                      {<<"user_id">>,<<"dmarion">>},
                      {<<"_links">>,
                       {[{<<"self">>,
                          {[{<<"href">>,
                             <<"http://192.168.33.66/api/v0/e/cd/bitbucket-servers/https%3A%2F%2F10.194.11.251">>}]}}]}}]}]},
                  {<<"project_key">>,BitbucketKey},
                  {<<"pipeline_branch">>,<<"master">>},
                  {<<"repo_name">>,BitbucketRepo}]}},
               {<<"name">>,<<"a_project">>}]},

    hoax:mock(deliv_db,
          ?expect(qfetch,
                  ?withArgs([deliv_project, convert_to_bb, [ProjId, BitbucketKey, BitbucketRepo]]),
                  ?andReturn({error, why}))),
    Result = deliv_project:convert_to_bb(Json, Proj),
    ?assertEqual({error, why}, Result),
    ?verifyAll.

convert_to_bb_when_qfetch_succeeds_returns_project() ->
    ProjId = <<"ProjId">>,
    Proj = deliv_project:fromlist([{id, ProjId}]),
    BitbucketKey = <<"DEMO">>,
    BitbucketRepo = <<"demo">>,
    Json = {[{<<"scm">>,
                {[{<<"type">>,<<"bitbucket">>},
                  {<<"projectCreateUri">>,
                   <<"/api/v0/e/cd/orgs/sandbox/bitbucket-projects">>},
                  {<<"scmSetupConfigs">>,
                   [{[{<<"root_api_url">>,
                       <<"https://10.194.11.251">>},
                      {<<"user_id">>,<<"dmarion">>},
                      {<<"_links">>,
                       {[{<<"self">>,
                          {[{<<"href">>,
                             <<"http://192.168.33.66/api/v0/e/cd/bitbucket-servers/https%3A%2F%2F10.194.11.251">>}]}}]}}]}]},
                  {<<"project_key">>,BitbucketKey},
                  {<<"pipeline_branch">>,<<"master">>},
                  {<<"repo_name">>,BitbucketRepo}]}},
               {<<"name">>,<<"a_project">>}]},

    hoax:mock(deliv_db,
          ?expect(qfetch,
                  ?withArgs([deliv_project, convert_to_bb, [ProjId, BitbucketKey, BitbucketRepo]]),
                  ?andReturn([Proj]))),
    Result = deliv_project:convert_to_bb(Json, Proj),
    ?assertEqual(Proj, Result),
    ?verifyAll.

convert_to_local_when_qfetch_succeeds_returns_project() ->
    ProjectId = 1,
    Project = deliv_project:fromlist([{id, ProjectId}, {scm_module, <<"bitbucket_scm">>}]),

    hoax:mock(deliv_db,
        ?expect(qfetch,
            ?withArgs([deliv_project, convert_to_local, [ProjectId]]),
            ?andReturn([Project]))),

    ?assertEqual(Project, deliv_project:convert_to_local(Project)),
    ?verifyAll.

convert_to_local_when_qfetch_fails_returns_an_error() ->
    ProjectId = 1,
    Project = deliv_project:fromlist([{id, ProjectId}, {scm_module, <<"bitbucket_scm">>}]),

    hoax:mock(deliv_db,
        ?expect(qfetch,
            ?withArgs([deliv_project, convert_to_local, [ProjectId]]),
            ?andReturn({error, why}))),

    ?assertEqual({error, why}, deliv_project:convert_to_local(Project)),
    ?verifyAll.

convert_ghv2_to_local_when_qfetch_succeeds_returns_project() ->
    ProjectId = 1,
    Project = deliv_project:fromlist([{id, ProjectId}, {scm_module, <<"bitbucket_scm">>}]),

    hoax:expect(receive
                    deliv_db:qfetch(deliv_project, convert_ghv2_to_local, [ProjectId]) -> [Project]
                end),

    ?assertEqual(Project, deliv_project:convert_ghv2_to_local(Project)),
    ?verifyAll.

convert_ghv2_to_local_when_qfetch_fails_returns_an_error() ->
    ProjectId = 1,
    Project = deliv_project:fromlist([{id, ProjectId}, {scm_module, <<"bitbucket_scm">>}]),

    hoax:expect(receive
                    deliv_db:qfetch(deliv_project, convert_ghv2_to_local, [ProjectId]) -> {error, why}
                end),

    ?assertEqual({error, why}, deliv_project:convert_ghv2_to_local(Project)),
    ?verifyAll.

convert_to_githubV2_when_qfetch_fails_returns_an_error() ->
    ProjId = <<"ProjId">>,
    Proj = deliv_project:fromlist([{id, ProjId}]),
    GitHubOwner = <<"DEMO">>,
    GitHubRepo = <<"demo">>,

    hoax:mock(deliv_db,
          ?expect(qfetch,
                  ?withArgs([deliv_project, convert_to_githubV2, [ProjId, GitHubOwner, GitHubRepo]]),
                  ?andReturn({error, why}))),
    Result = deliv_project:convert_to_githubV2(GitHubOwner, GitHubRepo, Proj),
    ?assertEqual({error, why}, Result),
    ?verifyAll.

convert_to_githubV2_when_qfetch_succeeds_returns_project() ->
    ProjId = <<"ProjId">>,
    Proj = deliv_project:fromlist([{id, ProjId}]),
    GitHubOwner= <<"DEMO">>,
    GitHubRepo= <<"demo">>,

    hoax:mock(deliv_db,
          ?expect(qfetch,
                  ?withArgs([deliv_project, convert_to_githubV2, [ProjId, GitHubOwner, GitHubRepo]]),
                  ?andReturn([Proj]))),
    Result = deliv_project:convert_to_githubV2(GitHubOwner, GitHubRepo, Proj),
    ?assertEqual(Proj, Result),
    ?verifyAll.

metadata_module_returns_the_project_metadata_module() ->
    LocalProj = deliv_project:fromlist([{scm_module, <<"deliv_scm_local">>}]),
    GitHubV1Proj = deliv_project:fromlist([{scm_module, <<"deliv_scm_github">>}]),
    GitHubV2Proj = deliv_project:fromlist([{scm_module, <<"github_scm">>}]),
    BitbucketProj = deliv_project:fromlist([{scm_module, <<"bitbucket_scm">>}]),

    ?assertEqual(none, deliv_project:metadata_module(LocalProj)),
    ?assertEqual(deliv_project_github_metadata, deliv_project:metadata_module(GitHubV1Proj)),
    ?assertEqual(scm_github_project_metadata, deliv_project:metadata_module(GitHubV2Proj)),
    ?assertEqual(scm_bitbucket_project_metadata, deliv_project:metadata_module(BitbucketProj)).

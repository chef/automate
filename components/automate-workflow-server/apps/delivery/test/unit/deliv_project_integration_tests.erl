-module(deliv_project_integration_tests).

-include_lib("hoax/include/hoax.hrl").
-include("deliv_coordinates.hrl").

-compile(export_all).

new_project_githubv1_test_() ->
    hoax:parameterized_fixture(?MODULE, "new_project_githubv1_", setupV1, teardown).

new_project_bb_test_() ->
    hoax:parameterized_fixture(?MODULE, "new_project_bb_", setupbb, teardown).

convert_ghv2_to_local_test_() ->
    hoax:parameterized_fixture(?MODULE, "convert_ghv2_to_local", setupV2, teardown).

delete_project_test_() ->
    hoax:parameterized_fixture(?MODULE, "delete", setupV2, teardown).

setupV2() ->
    error_logger:tty(false),
    eu_database:setup(),
    eu_data:with_enterprise(<<"scm_github_project_metadata_ent">>,
        eu_data:with_organization(<<"scm_github_project_metadata_org">>,
            eu_data:with_githubV2_project(<<"scm_github_project_metadata_project">>, <<"master">>, <<"gh_owner">>, <<"gh_repo">>,
                fun(Enterprise, Organization, Project) ->
                    {Enterprise, Organization, Project}
                end))).

setupV1() ->
    error_logger:tty(false),
    eu_database:setup(),
    eu_data:with_enterprise(<<"scm_github_project_metadata_ent">>,
        eu_data:with_organization(<<"scm_github_project_metadata_org">>,
            eu_data:with_github_project(<<"scm_github_project_metadata_project">>, <<"master">>, <<"gh_owner">>, <<"gh_repo">>,
                fun(Enterprise, Organization, Project) ->
                    {Enterprise, Organization, Project}
                end))).

setupbb() ->
    error_logger:tty(false),
    eu_database:setup(),
    eu_data:with_enterprise(<<"scm_bitbucket_project_metadata_ent">>,
        eu_data:with_organization(<<"scm_bitbucket_project_metadata_ent">>,
            eu_data:with_bitbucket_project(<<"scm_bitbucket_project_metadata_project">>, <<"master">>, <<"BBKY">>, <<"bb_repo">>,
                fun(Enterprise, Organization, Project) ->
                    {Enterprise, Organization, Project}
                end))).

teardown(_) ->
    eu_database:teardown(),
    error_logger:tty(true).

new_project_githubv1_creates_project({Enterprise, Organization, Project}) ->
    EntName = deliv_enterprise:getval(name, Enterprise),
    OrgName = deliv_organization:getval(name, Organization),
    ProjName = deliv_project:getval(name, Project),

    {ok, NewProject} = deliv_project:fetch(EntName, OrgName, ProjName),
    ?assertEqual(Project, NewProject).

new_project_githubv1_creates_pipeline({Enterprise, Organization, Project}) ->
    EntName = deliv_enterprise:getval(name, Enterprise),
    OrgName = deliv_organization:getval(name, Organization),
    ProjName = deliv_project:getval(name, Project),
    PipeName = <<"master">>,

    {ok, Pipeline} = deliv_pipeline:fetch(EntName, OrgName, ProjName, PipeName),
    ?assertEqual(PipeName, deliv_pipeline:getval(name, Pipeline)).

new_project_githubv1_creates_metadata({_Enterprise, _Organization, Project}) ->
    RepoName = <<"gh_repo">>,
    RepoOwner = <<"gh_owner">>,
    {ok, GithubMetaData} = deliv_project_github_metadata:fetch_by_id(deliv_project:getval(id, Project)),
    ?assertEqual(RepoOwner, deliv_project_github_metadata:getval(repo_owner, GithubMetaData)),
    ?assertEqual(RepoName, deliv_project_github_metadata:getval(repo_name, GithubMetaData)).

new_project_bb_creates_project({Enterprise, Organization, Project}) ->
    EntName = deliv_enterprise:getval(name, Enterprise),
    OrgName = deliv_organization:getval(name, Organization),
    ProjName = deliv_project:getval(name, Project),

    {ok, NewProject} = deliv_project:fetch(EntName, OrgName, ProjName),
    ?assertEqual(Project, NewProject).

new_project_bb_creates_pipeline({Enterprise, Organization, Project}) ->
    EntName = deliv_enterprise:getval(name, Enterprise),
    OrgName = deliv_organization:getval(name, Organization),
    ProjName = deliv_project:getval(name, Project),
    PipeName = <<"master">>,

    {ok, Pipeline} = deliv_pipeline:fetch(EntName, OrgName, ProjName, PipeName),
    ?assertEqual(PipeName, deliv_pipeline:getval(name, Pipeline)).

new_project_bb_creates_metadata({_Enterprise, _Organization, Project}) ->
    RepoName =  <<"bb_repo">>,
    BitbucketProject =  <<"BBKY">>,
    {ok, BitbucketMetaData} = scm_bitbucket_project_metadata:fetch_by_id(deliv_project:getval(id, Project)),
    ?assertEqual(BitbucketProject, scm_bitbucket_project_metadata:getval(bitbucket_project, BitbucketMetaData)),
    ?assertEqual(RepoName, scm_bitbucket_project_metadata:getval(repo_name, BitbucketMetaData)).


convert_ghv2_to_local_changes_scm_module_to_local({_Enterprise, _Organization, Project}) ->
    ProjectId = deliv_project:getval(id, Project),
    Project2 = deliv_project:convert_ghv2_to_local(Project),

    ?assertEqual(ProjectId, deliv_project:getval(id, Project2)),
    ?assertEqual(<<"deliv_scm_local">>, deliv_project:getval(scm_module, Project2)).

delete_project_deletes_the_project({_Enterprise, _Organization, Project}) ->
    ProjectId = deliv_project:getval(id, Project),
    ok = deliv_project:delete(Project),
    ?assertEqual({error, not_found}, deliv_project:fetch_by_id(ProjectId)).

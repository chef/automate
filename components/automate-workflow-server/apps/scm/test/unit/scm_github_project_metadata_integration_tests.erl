-module(scm_github_project_metadata_integration_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_coordinates.hrl").

-compile(export_all).

scm_github_project_metadata_integration_test_() ->
    [
     hoax:parameterized_fixture(?MODULE, "v2_fetch_", setupV2, teardown),
     hoax:parameterized_fixture(?MODULE, "v2_update_", setupV2, teardown),
     hoax:parameterized_fixture(?MODULE, "v1_fetch_", setupV1, teardown)
    ].

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

teardown(_) ->
    eu_database:teardown(),
    error_logger:tty(true).


v2_fetch_all_for_ent_name_url_returns_a_githubV2_integrated_project({Enterprise, _Organization, Project}) ->
    ProjId = deliv_project:getval(id, Project),
    EntId = deliv_enterprise:getval(id, Enterprise),
    EntName = deliv_enterprise:getval(name, Enterprise),
    Url = <<"http://mock.url">>,

    scm_basic_auth:save_basic_auth_credentials(Url,
                                                     <<"mockuserid">>,
                                                     <<"mockpw">>,
                                                     EntId,
                                                     <<"github">>),
    {ok, [Metadata]} = scm_github_project_metadata:fetch_all_for_ent_name_url(EntName, Url),

    ?assertEqual(ProjId, scm_github_project_metadata:getval(project_id, Metadata)),
    ?assertEqual(<<"gh_owner">>, scm_github_project_metadata:getval(repo_owner, Metadata)),
    ?assertEqual(<<"gh_repo">>, scm_github_project_metadata:getval(repo_name, Metadata)).

v1_fetch_all_for_ent_name_url_returns_error_not_found_when_only_a_githubV1_project_exists({Enterprise, _Organization, _Project}) ->
    EntId = deliv_enterprise:getval(id, Enterprise),
    EntName = deliv_enterprise:getval(name, Enterprise),
    Url = <<"http://mock.url">>,

    scm_basic_auth:save_basic_auth_credentials(Url,
                                                     <<"mockuserid">>,
                                                     <<"mockpw">>,
                                                     EntId,
                                                     <<"github">>),
    ?assertEqual({error, not_found}, scm_github_project_metadata:fetch_all_for_ent_name_url(EntName, Url)).

v2_update_by_id_returns_updated_project_metadata({_Ent, _Org, Proj}) ->
    ProjId = deliv_project:getval(id, Proj),
    RepoOwner = <<"Hank">>,
    RepoName = <<"Venture">>,

    {ok, Metadata} = scm_github_project_metadata:update_by_id(ProjId, RepoOwner, RepoName),
    ?assertEqual(ProjId, scm_github_project_metadata:getval(project_id, Metadata)),
    ?assertEqual(RepoOwner, scm_github_project_metadata:getval(repo_owner, Metadata)),
    ?assertEqual(RepoName, scm_github_project_metadata:getval(repo_name, Metadata)).

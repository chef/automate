-module(convert_scm_integration_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_coordinates.hrl").
-include("../../src/scm_types.hrl").

-compile(export_all).

bitbucket_to_local_test_() ->
    hoax:parameterized_fixture(?MODULE, "bitbucket_to_local", setup_bitbucket, teardown).

convert_to_test_() ->
    hoax:parameterized_fixture(?MODULE, "convert_to_", setup, teardown).

setup_bitbucket() ->
    error_logger:tty(false),
    eu_database:setup(),
    eu_data:with_enterprise(<<"scm_bitbucket_project_metadata_ent">>,
        eu_data:with_organization(<<"scm_bitbucket_project_metadata_org">>,
            eu_data:with_bitbucket_project(<<"scm_bitbucket_project_metadata_project">>, <<"master">>,
              <<"BBKY">>, <<"bb_repo">>,
                fun(Enterprise, Organization, Project) ->
                    {Enterprise, Organization, Project}
                end))).

setup() ->
    error_logger:tty(false),
    eu_database:setup(),
    eu_data:with_enterprise(<<"scm_github_project_metadata_ent">>,
        eu_data:with_organization(<<"scm_github_project_metadata_org">>,
            eu_data:with_project(<<"scm_github_project_metadata_project">>,
                eu_data:with_pipeline(<<"master">>,
                fun(Enterprise, Organization, Project, _Pipeline) ->
                    {Enterprise, Organization, Project}
                end)))).

teardown(_) ->
    eu_database:teardown(),
    error_logger:tty(true).

convert_to_bb_converts_to_bb_and_updates_metadata_table({Enterprise, Organization, Project}) ->
     EntName = deliv_enterprise:getval(name, Enterprise),
     OrgName = deliv_organization:getval(name, Organization),
     ProjName = deliv_project:getval(name, Project),
     ScmRepo = <<"scm_repo">>,
     Json = {[{<<"scm">>,
               [
                 {<<"project_key">>, ProjName},
                 {<<"repo_name">>, ScmRepo}
               ]
               }]},
     Coords = #proj_coordinates{ ent_name = EntName,
                                 org_name = OrgName,
                                 proj_name = ProjName},

     Result = deliv_project:convert_to_bb(Json, Project),

     ?assertEqual(<<"bitbucket_scm">>, deliv_project:getval(scm_module, Result)),

     % assert that the expected data is inserted into bitbucket_project_metadata table
     {ok, BBMetadata} = scm_bitbucket_project_metadata:fetch_by_project_coords(Coords),
     ?assertEqual(ScmRepo, scm_bitbucket_project_metadata:getval(repo_name, BBMetadata)),
     ?assertEqual(ProjName, scm_bitbucket_project_metadata:getval(bitbucket_project, BBMetadata)),
     ?assertEqual(deliv_project:getval(id, Result), scm_bitbucket_project_metadata:getval(project_id, BBMetadata)).

bitbucket_to_local_converts_to_local_and_removes_entry_from_metadata_table({Enterprise, Organization, Project}) ->
    EntName = deliv_enterprise:getval(name, Enterprise),
    OrgName = deliv_organization:getval(name, Organization),
    ProjName = deliv_project:getval(name, Project),
    ProjId = deliv_project:getval(id, Project),
    LocalProject = deliv_project:convert_to_local(Project),
    Coords = #proj_coordinates{ ent_name = EntName,
                                 org_name = OrgName,
                                 proj_name = ProjName},

    ?assertEqual({error, not_found}, scm_bitbucket_project_metadata:fetch_scm_metadata_by_coords(Coords)),
    ?assertEqual(ProjId, deliv_project:getval(id, LocalProject)).

convert_to_githubV2_converts_to_githubV2_and_updates_metadata_table({Enterprise, Organization, Project}) ->
     EntName = deliv_enterprise:getval(name, Enterprise),
     OrgName = deliv_organization:getval(name, Organization),
     ProjName = deliv_project:getval(name, Project),
     RepoOwner = <<"scm_owner">>,
     RepoName = <<"scm_repo">>,

     Coords = #proj_coordinates{ ent_name = EntName,
                                 org_name = OrgName,
                                 proj_name = ProjName},

     Result = deliv_project:convert_to_githubV2(RepoOwner, RepoName, Project),

     ?assertEqual(<<"github_scm">>, deliv_project:getval(scm_module, Result)),

     % assert that the expected data is inserted into bitbucket_project_metadata table
     {ok, #metadata_by_scm{repo_name = InsertedRepoName, repo_group = InsertedRepoOwner}} = scm_bitbucket_project_metadata:fetch_scm_metadata_by_coords(Coords),
     ?assertEqual(RepoName, InsertedRepoName),
     ?assertEqual(RepoOwner, InsertedRepoOwner),
     ?assertEqual(deliv_project:getval(id, Project), deliv_project:getval(id, Result)).

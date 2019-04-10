-module(scm_bitbucket_project_metadata_integration_tests).

-include_lib("hoax/include/hoax.hrl").

-compile(export_all).

hoax_fixture_test_ () ->
    hoax:parameterized_fixture(?MODULE, setup, teardown).

setup() ->
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

fetch_by_id({_Enterprise, _Organization, Project}) ->
    ProjId = deliv_project:getval(id, Project),
    {ok, Metadata} = scm_bitbucket_project_metadata:fetch_by_id(ProjId),

    ?assertEqual(ProjId, scm_bitbucket_project_metadata:getval(project_id, Metadata)),
    ?assertEqual(<<"BBKY">>, scm_bitbucket_project_metadata:getval(bitbucket_project, Metadata)),
    ?assertEqual(<<"bb_repo">>, scm_bitbucket_project_metadata:getval(repo_name, Metadata)).

fetch_by_project_coords({_Enterprise, _Organization, Project}) ->
    ProjId = deliv_project:getval(id, Project),
    Coords = deliv_project:to_coords(Project),
    {ok, Metadata} = scm_bitbucket_project_metadata:fetch_by_project_coords(Coords),

    ?assertEqual(ProjId, scm_bitbucket_project_metadata:getval(project_id, Metadata)),
    ?assertEqual(<<"BBKY">>, scm_bitbucket_project_metadata:getval(bitbucket_project, Metadata)),
    ?assertEqual(<<"bb_repo">>, scm_bitbucket_project_metadata:getval(repo_name, Metadata)).

fetch_all_for_ent_name_url({Enterprise, _Organization, Project}) ->
    ProjId = deliv_project:getval(id, Project),
    EntId = deliv_enterprise:getval(id, Enterprise),
    EntName = deliv_enterprise:getval(name, Enterprise),
    Url = <<"http://mock.url">>,

    scm_basic_auth:save_basic_auth_credentials(Url,
                                                     <<"mockuserid">>,
                                                     <<"mockpw">>,
                                                     EntId,
                                                     <<"bitbucket">>),
    {ok, [Metadata]} = scm_bitbucket_project_metadata:fetch_all_for_ent_name_url(EntName, Url),

    ?assertEqual(ProjId, scm_bitbucket_project_metadata:getval(project_id, Metadata)),
    ?assertEqual(<<"BBKY">>, scm_bitbucket_project_metadata:getval(bitbucket_project, Metadata)),
    ?assertEqual(<<"bb_repo">>, scm_bitbucket_project_metadata:getval(repo_name, Metadata)).

update_by_id({_Enterprise, _Organization, Project}) ->
    ProjId = deliv_project:getval(id, Project),
    {ok, Metadata} = scm_bitbucket_project_metadata:update_by_id(ProjId, <<"update_bb_proj">>, <<"update_bb_repo">>),

    ?assertEqual(ProjId, scm_bitbucket_project_metadata:getval(project_id, Metadata)),
    ?assertEqual(<<"update_bb_proj">>, scm_bitbucket_project_metadata:getval(bitbucket_project, Metadata)),
    ?assertEqual(<<"update_bb_repo">>, scm_bitbucket_project_metadata:getval(repo_name, Metadata)),

    {ok, Metadata2} = scm_bitbucket_project_metadata:fetch_by_id(ProjId),

    ?assertEqual(ProjId, scm_bitbucket_project_metadata:getval(project_id, Metadata2)),
    ?assertEqual(<<"update_bb_proj">>, scm_bitbucket_project_metadata:getval(bitbucket_project, Metadata2)),
    ?assertEqual(<<"update_bb_repo">>, scm_bitbucket_project_metadata:getval(repo_name, Metadata2)).

-module(scm_bitbucket_project_metadata_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_coordinates.hrl").

-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test Fixtures
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fetch_by_id_test_() ->
    hoax:fixture(?MODULE, "fetch_by_id_").

fetch_by_project_coords_test_() ->
    hoax:fixture(?MODULE, "fetch_by_project_coords_").

fetch_scm_test_() ->
    hoax:fixture(?MODULE, "fetch_scm").

fetch_all_by_ent_name_url_test_() ->
    hoax:fixture(?MODULE, "fetch_all_by_ent_name_url_").

update_by_id_test_() ->
    hoax:fixture(?MODULE, "update_by_id_").

inject_json_test_() ->
    hoax:fixture(?MODULE, "inject_json_").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test Cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fetch_by_id_returns_metadata_when_no_db_errors() ->
    ProjId = 1,
    ProjectMetaData = scm_bitbucket_project_metadata:fromlist([{project_id, ProjId},
                                                           {bitbucket_project, <<"bb_proj">>},
                                                           {repo_name, <<"bb_repo">>}]),

    hoax:mock(deliv_db,
              ?expect(fetch_by_id,
                      ?withArgs([scm_bitbucket_project_metadata, ProjId]),
                      ?andReturn({ok, ProjectMetaData}))),

    ?assertEqual({ok, ProjectMetaData},
                 scm_bitbucket_project_metadata:fetch_by_id(ProjId)),
    ?verifyAll.

fetch_by_id_returns_error_not_found_when_metadata_is_not_found() ->
    ProjId = 1,

    hoax:mock(deliv_db,
              ?expect(fetch_by_id,
                      ?withArgs([scm_bitbucket_project_metadata, ProjId]),
                      ?andReturn({error, not_found}))),

    ?assertEqual({error, not_found},
                 scm_bitbucket_project_metadata:fetch_by_id(ProjId)),
    ?verifyAll.

fetch_by_id_forwards_other_db_errors() ->
    ProjId = 1,

    hoax:mock(deliv_db,
              ?expect(fetch_by_id,
                      ?withArgs([scm_bitbucket_project_metadata, ProjId]),
                      ?andReturn({error, database_go_boom}))),

    ?assertEqual({error, database_go_boom},
                 scm_bitbucket_project_metadata:fetch_by_id(ProjId)),
    ?verifyAll.

fetch_by_project_coords_return_proj_repo() ->
    EntName = <<"Ent">>,
    OrgName = <<"Org">>,
    ProjName = <<"Proj">>,

    ProjectCoords = #proj_coordinates{ent_name = EntName,
                                      org_name = OrgName,
                                      proj_name = ProjName},
    BitbucketProject = <<"BitbucketProject">>,
    RepoName  = <<"RepoName">>,
    ProjectMetaData = scm_bitbucket_project_metadata:fromlist([{bitbucket_project, BitbucketProject},
                                                           {repo_name, RepoName}]),

    hoax:mock(deliv_db,
              ?expect(qfetch,
                      ?withArgs([scm_bitbucket_project_metadata,
                                 fetch_by_project_coords,
                                 [EntName, OrgName, ProjName]]),
                      ?andReturn([ProjectMetaData]))),
    ?assertEqual({ok, ProjectMetaData},
                 scm_bitbucket_project_metadata:fetch_by_project_coords(ProjectCoords)),
    ?verifyAll.

fetch_by_project_coords_returns_an_error_if_not_found() ->
    EntName = <<"Ent">>,
    OrgName = <<"Org">>,
    ProjName = <<"Proj">>,

    ProjectCoords = #proj_coordinates{ent_name = EntName,
                                      org_name = OrgName,
                                      proj_name = ProjName},
    hoax:mock(deliv_db,
              ?expect(qfetch,
                      ?withArgs([scm_bitbucket_project_metadata,
                                 fetch_by_project_coords,
                                 [EntName, OrgName, ProjName]]),
                      ?andReturn([]))),
    %% If no metadata is found, that's OK, and we don't expect to log any
    %% errors
    hoax:mock(chef_log,
              ?expect(error, ?withArgs([?any, ?any]), ?times(0))),

    ?assertEqual({error, not_found},
                 scm_bitbucket_project_metadata:fetch_by_project_coords(ProjectCoords)),
    ?verifyAll.

fetch_by_project_coords_logs_database_query_errors() ->
    EntName = <<"Ent">>,
    OrgName = <<"Org">>,
    ProjName = <<"Proj">>,

    ProjectCoords = #proj_coordinates{ent_name = EntName,
                                      org_name = OrgName,
                                      proj_name = ProjName},

    hoax:mock(deliv_db,
              ?expect(qfetch,
                      ?withArgs([scm_bitbucket_project_metadata,
                                 fetch_by_project_coords,
                                 [EntName, OrgName, ProjName]]),
                      ?andReturn({error, database_go_boom}))),

    %% We *should* log a database error
    hoax:mock(chef_log,
              ?expect(failed_call,
                      ?withArgs([scm_bitbucket_project_metadata, fetch_by_project_coords, [ProjectCoords], database_go_boom]))),

    %% But that database error doesn't get surfaced to the caller
    %% (nothing they can do about it)
    ?assertEqual({error, not_found},
                 scm_bitbucket_project_metadata:fetch_by_project_coords(ProjectCoords)),
    ?verifyAll.

fetch_all_by_ent_name_url_returns_metadatas_when_found() ->
    EntName = <<"Ent">>,
    Url = <<"http://mock.url">>,

    BitbucketProject = <<"BitbucketProject">>,
    RepoName  = <<"RepoName">>,
    ProjectMetaData = scm_bitbucket_project_metadata:fromlist([{bitbucket_project, BitbucketProject},
                                                           {repo_name, RepoName}]),

    hoax:mock(deliv_db,
              ?expect(qfetch,
                      ?withArgs([scm_bitbucket_project_metadata,
                                 fetch_all_for_ent_name_url,
                                 [EntName, Url]]),
                      ?andReturn([ProjectMetaData]))),
    ?assertEqual({ok, [ProjectMetaData]},
                 scm_bitbucket_project_metadata:fetch_all_for_ent_name_url(EntName, Url)),
    ?verifyAll.

fetch_all_by_ent_name_url_returns_error_not_found_when_not_found() ->
    EntName = <<"Ent">>,
    Url = <<"http://mock.url">>,

    hoax:mock(deliv_db,
              ?expect(qfetch,
                      ?withArgs([scm_bitbucket_project_metadata,
                                 fetch_all_for_ent_name_url,
                                 [EntName, Url]]),
                      ?andReturn([]))),
    ?assertEqual({error, not_found},
                 scm_bitbucket_project_metadata:fetch_all_for_ent_name_url(EntName, Url)),
    ?verifyAll.

fetch_all_by_ent_name_url_forwards_other_errors() ->
    EntName = <<"Ent">>,
    Url = <<"http://mock.url">>,

    hoax:mock(deliv_db,
              ?expect(qfetch,
                      ?withArgs([scm_bitbucket_project_metadata,
                                 fetch_all_for_ent_name_url,
                                 [EntName, Url]]),
                      ?andReturn({error, db_go_boom}))),
    ?assertEqual({error, db_go_boom},
                 scm_bitbucket_project_metadata:fetch_all_for_ent_name_url(EntName, Url)),
    ?verifyAll.

update_by_id_updates_and_returns_ok_metadata_when_no_db_errors() ->
    ProjId = 1,
    BBProj = <<"bb_proj">>,
    BBRepo = <<"bb_repo">>,
    ProjectMetaData = scm_bitbucket_project_metadata:fromlist([{project_id, ProjId},
                                                           {bitbucket_project, <<"bb_proj">>},
                                                           {repo_name, <<"bb_repo">>}]),

    hoax:mock(deliv_db,
              ?expect(qfetch,
                      ?withArgs([scm_bitbucket_project_metadata, update_by_id, [ProjId, BBProj, BBRepo]]),
                      ?andReturn([ProjectMetaData]))),

    ?assertEqual({ok, ProjectMetaData},
                 scm_bitbucket_project_metadata:update_by_id(ProjId, BBProj, BBRepo)),
    ?verifyAll.

update_by_id_returns_error_not_found_when_metadata_is_not_found() ->
    ProjId = 1,
    BBProj = <<"bb_proj">>,
    BBRepo = <<"bb_repo">>,

    hoax:mock(deliv_db,
              ?expect(qfetch,
                      ?withArgs([scm_bitbucket_project_metadata, update_by_id, [ProjId, BBProj, BBRepo]]),
                      ?andReturn([]))),

    ?assertEqual({error, not_found},
                 scm_bitbucket_project_metadata:update_by_id(ProjId, BBProj, BBRepo)),
    ?verifyAll.

update_by_id_forwards_other_db_errors() ->
    ProjId = 1,
    BBProj = <<"bb_proj">>,
    BBRepo = <<"bb_repo">>,

    hoax:mock(deliv_db,
              ?expect(qfetch,
                      ?withArgs([scm_bitbucket_project_metadata, update_by_id, [ProjId, BBProj, BBRepo]]),
                      ?andReturn({error, database_go_boom}))),

    ?assertEqual({error, database_go_boom},
                 scm_bitbucket_project_metadata:update_by_id(ProjId, BBProj, BBRepo)),
    ?verifyAll.


fetch_scm_metadata_by_coords_logs_and_returns_error_if_multiple_configs_found() ->
    EntName = <<"Ent">>,
    OrgName = <<"Org">>,
    ProjName = <<"Proj">>,

    ProjectCoords = #proj_coordinates{ent_name = EntName,
                                      org_name = OrgName,
                                      proj_name = ProjName},

    hoax:expect(
                receive
                    deliv_db:select(scm_bitbucket_project_metadata,
                                 fetch_scm_metadata_by_project,
                                 [EntName, OrgName, ProjName],
                                 rows_as_records,
                                 [metadata_by_scm,
                                 [scm_type, repo_name, repo_group]]) -> {ok, [metadata1, metadata2]}
                end,
                chef_log:failed_call(scm_bitbucket_project_metadata,
                                     fetch_scm_metadata_by_coords,
                                     [ProjectCoords],
                                     multiple_scm_configs)
                ),

    ?assertEqual({error, multiple_scm_configs},
               scm_bitbucket_project_metadata:fetch_scm_metadata_by_coords(ProjectCoords)),
    ?verifyAll.

fetch_scm_metadata_by_coords_returns_not_found_if_no_metadata() ->
    EntName = <<"Ent">>,
    OrgName = <<"Org">>,
    ProjName = <<"Proj">>,

    ProjectCoords = #proj_coordinates{ent_name = EntName,
                                      org_name = OrgName,
                                      proj_name = ProjName},

    hoax:expect(
                receive
                    deliv_db:select(scm_bitbucket_project_metadata,
                                 fetch_scm_metadata_by_project,
                                 [EntName, OrgName, ProjName],
                                 rows_as_records,
                                 [metadata_by_scm,
                                 [scm_type, repo_name, repo_group]]) -> {ok, []}
                end,
                chef_log:failed_call(scm_bitbucket_project_metadata,
                                     fetch_scm_metadata_by_coords,
                                     [ProjectCoords],
                                     not_found)
                ),

    ?assertEqual({error, not_found},
               scm_bitbucket_project_metadata:fetch_scm_metadata_by_coords(ProjectCoords)),
    ?verifyAll.

fetch_scm_metadata_by_coords_returns_ok_metadata_when_found() ->
    EntName = <<"Ent">>,
    OrgName = <<"Org">>,
    ProjName = <<"Proj">>,

    ProjectCoords = #proj_coordinates{ent_name = EntName,
                                      org_name = OrgName,
                                      proj_name = ProjName},

    hoax:expect(
                receive
                    deliv_db:select(scm_bitbucket_project_metadata,
                                 fetch_scm_metadata_by_project,
                                 [EntName, OrgName, ProjName],
                                 rows_as_records,
                                 [metadata_by_scm,
                                 [scm_type, repo_name, repo_group]]) -> {ok, [metadata]}
                end
                ),

    ?assertEqual({ok, metadata},
               scm_bitbucket_project_metadata:fetch_scm_metadata_by_coords(ProjectCoords)),
    ?verifyAll.

fetch_scm_metadata_by_coords_returns_error_if_db_error() ->
    EntName = <<"Ent">>,
    OrgName = <<"Org">>,
    ProjName = <<"Proj">>,

    ProjectCoords = #proj_coordinates{ent_name = EntName,
                                      org_name = OrgName,
                                      proj_name = ProjName},

    hoax:expect(
                receive
                    deliv_db:select(scm_bitbucket_project_metadata,
                                 fetch_scm_metadata_by_project,
                                 [EntName, OrgName, ProjName],
                                 rows_as_records,
                                 [metadata_by_scm,
                                 [scm_type, repo_name, repo_group]]) -> {error, oh_noooooooooo}
                end
                ),

    ?assertEqual({error, oh_noooooooooo},
               scm_bitbucket_project_metadata:fetch_scm_metadata_by_coords(ProjectCoords)),
    ?verifyAll.

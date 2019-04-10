-module(deliv_project_github_metadata_tests).

-include_lib("hoax/include/hoax.hrl").

-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test Fixtures
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
client_details_test_() ->
    hoax:fixture(?MODULE, "client_details").

fetch_by_id_test_() ->
    hoax:fixture(?MODULE, "fetch_by_id_").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test Cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
client_details_return_org_repo_and_token() ->
    ProjectId = 123,
    RepoOwner = <<"RepoOwner">>,
    RepoName  = <<"RepoName">>,
    hoax:mock(deliv_db,
              ?expect(fetch2,
                      ?withArgs([deliv_project_github_metadata,
                                 project_id,
                                 ProjectId]),
                      ?andReturn({ok, [deliv_project_github_metadata:fromlist([{project_id, ProjectId},
                                                                               {repo_owner, RepoOwner},
                                                                               {repo_name, RepoName}])]}))),
    ?assertEqual({ok, RepoOwner, RepoName},
                 deliv_project_github_metadata:client_details(ProjectId)),
    ?verifyAll.

client_details_returns_an_error_if_no_token_found() ->
    ProjectId = 123,
    hoax:mock(deliv_db,
              ?expect(fetch2,
                      ?withArgs([deliv_project_github_metadata,
                                 project_id,
                                 ProjectId]),
                      ?andReturn({ok, []}))),
    %% If no token is found, that's OK, and we don't expect to log any
    %% errors
    hoax:mock(chef_log,
              ?expect(error, ?withArgs([?any, ?any]), ?times(0))),

    ?assertEqual({error, not_found},
                 deliv_project_github_metadata:client_details(ProjectId)),
    ?verifyAll.

client_details_logs_database_query_errors() ->
    ProjectId = 123,
    hoax:mock(deliv_db,
              ?expect(fetch2,
                      ?withArgs([deliv_project_github_metadata,
                                 project_id,
                                 ProjectId]),
                      ?andReturn({error, database_go_boom}))),

    %% We *should* log a database error
    hoax:mock(chef_log,
              ?expect(failed_call,
                      ?withArgs([deliv_project_github_metadata, client_details, [123], database_go_boom]))),

    %% But that database error doesn't get surfaced to the caller
    %% (nothing they can do about it)
    ?assertEqual({error, not_found},
                 deliv_project_github_metadata:client_details(ProjectId)),
    ?verifyAll.

fetch_by_id_returns_metadata_when_no_db_errors() ->
    ProjId = 1,
    ProjectMetaData = scm_bitbucket_project_metadata:fromlist([{project_id, ProjId},
                                                               {repo_owner, <<"github_owner">>},
                                                               {repo_name, <<"github_repo">>}]),
    hoax:mock(deliv_db,
              ?expect(fetch_by_id,
                      ?withArgs([deliv_project_github_metadata, ProjId]),
                      ?andReturn({ok, ProjectMetaData}))),

    ?assertEqual({ok, ProjectMetaData},
                deliv_project_github_metadata:fetch_by_id(ProjId)),
    ?verifyAll.

fetch_by_id_returns_error_not_found_when_metadata_is_not_found() ->
    ProjId = 1,

    hoax:mock(deliv_db,
              ?expect(fetch_by_id,
                      ?withArgs([deliv_project_github_metadata, ProjId]),
                      ?andReturn({error, not_found}))),

    ?assertEqual({error, not_found},
                 deliv_project_github_metadata:fetch_by_id(ProjId)),
    ?verifyAll.

fetch_by_id_forwards_other_db_errors() ->
    ProjId = 1,

    hoax:mock(deliv_db,
              ?expect(fetch_by_id,
                      ?withArgs([deliv_project_github_metadata, ProjId]),
                      ?andReturn({error, database_go_boom}))),

    ?assertEqual({error, database_go_boom},
                 deliv_project_github_metadata:fetch_by_id(ProjId)),
    ?verifyAll.

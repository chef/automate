-module(scm_github_project_metadata_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_coordinates.hrl").

-compile(export_all).


scm_github_project_metadata_test_() ->
    [
        hoax:fixture(?MODULE, 'fetch_all_by_ent_name_url_'),
        hoax:fixture(?MODULE, 'update_by_id_'),
        hoax:fixture(?MODULE, 'fetch_by_id_')
    ].

fetch_all_by_ent_name_url_returns_metadatas_when_found() ->
    EntName = <<"Ent">>,
    Url = <<"http://mock.url">>,

    GithubProject = <<"GithubProject">>,
    RepoName  = <<"RepoName">>,
    ProjectMetaData = scm_github_project_metadata:fromlist([{github_project, GithubProject},
                                                           {repo_name, RepoName}]),

    hoax:mock(deliv_db,
              ?expect(qfetch,
                      ?withArgs([scm_github_project_metadata,
                                 fetch_all_for_ent_name_url,
                                 [EntName, Url]]),
                      ?andReturn([ProjectMetaData]))),
    ?assertEqual({ok, [ProjectMetaData]},
                 scm_github_project_metadata:fetch_all_for_ent_name_url(EntName, Url)),
    ?verifyAll.

fetch_all_by_ent_name_url_returns_error_not_found_when_no_metadatas_returned() ->
    EntName = <<"Ent">>,
    Url = <<"https://github.com">>,

    hoax:mock(deliv_db,
              ?expect(qfetch,
                      ?withArgs([scm_github_project_metadata,
                                 fetch_all_for_ent_name_url,
                                 [EntName, Url]]),
                      ?andReturn([]))),
    ?assertEqual({error, not_found}, scm_github_project_metadata:fetch_all_for_ent_name_url(EntName, Url)),
    ?verifyAll.

fetch_all_by_ent_name_url_logs_and_returns_error_when_qfetch_errors() ->
    EntName = <<"Ent">>,
    Url = <<"https://github.com">>,
    Why = whyyyyyy,

    hoax:mock(deliv_db,
              ?expect(qfetch,
                      ?withArgs([scm_github_project_metadata,
                                 fetch_all_for_ent_name_url,
                                 [EntName, Url]]),
                      ?andReturn({error, Why}))),
    hoax:mock(chef_log,
              ?expect(failed_call,
                      ?withArgs([scm_github_project_metadata,
                                 fetch_all_for_ent_name_url,
                                 [EntName, Url],
                                 Why]))),

    ?assertEqual({error, Why}, scm_github_project_metadata:fetch_all_for_ent_name_url(EntName, Url)),
    ?verifyAll.

fetch_by_id_returns_metadata_when_no_db_errors() ->
    ProjId = 1,

    hoax:expect(receive
                deliv_db:fetch_by_id(scm_github_project_metadata, ProjId) -> {ok, project_metadata}
            end),

    ?assertEqual({ok, project_metadata},
                scm_github_project_metadata:fetch_by_id(ProjId)),
    ?verifyAll.

fetch_by_id_returns_error_not_found_when_metadata_is_not_found() ->
    ProjId = 1,

    hoax:expect(receive
                deliv_db:fetch_by_id(scm_github_project_metadata, ProjId) -> {error, not_found}
            end),

    Result = scm_github_project_metadata:fetch_by_id(ProjId),

    ?assertEqual({error, not_found}, Result),
    ?verifyAll.

fetch_by_id_forwards_other_db_errors() ->
    ProjId = 1,

    hoax:expect(receive
                deliv_db:fetch_by_id(scm_github_project_metadata, ProjId) -> {error, database_go_boom}
            end),

    Result = scm_github_project_metadata:fetch_by_id(ProjId),

    ?assertEqual({error, database_go_boom}, Result),
    ?verifyAll.

update_by_id_when_returns_error_logs_failed_call_and_returns_error() ->
    ProjId = <<"IDIDIDID">>,
    RepoOwner = <<"DeanVenture">>,
    RepoName = <<"SpeedSuit">>,
    Why = nononononono,

    hoax:expect(receive
                  deliv_db:qfetch(scm_github_project_metadata, update_by_id, [ProjId, RepoOwner, RepoName]) -> {error, Why};
                  chef_log:failed_call(scm_github_project_metadata, update_by_id, [ProjId, RepoOwner, RepoName], Why) -> ignored
                end),

    Result = scm_github_project_metadata:update_by_id(ProjId, RepoOwner, RepoName),
    ?assertEqual({error, Why}, Result),
    ?verifyAll.

update_by_id_when_returns_empty_list_returns_error_not_found() ->
    ProjId = <<"IDIDIDID">>,
    RepoOwner = <<"DeanVenture">>,
    RepoName = <<"SpeedSuit">>,
    Why = not_found,

    hoax:expect(receive
                  deliv_db:qfetch(scm_github_project_metadata, update_by_id, [ProjId, RepoOwner, RepoName]) -> []
                end),

    Result = scm_github_project_metadata:update_by_id(ProjId, RepoOwner, RepoName),
    ?assertEqual({error, Why}, Result),
    ?verifyAll.

update_by_id_when_updates_project_returns_ok_project() ->
    ProjId = <<"IDIDIDID">>,
    RepoOwner = <<"DeanVenture">>,
    RepoName = <<"SpeedSuit">>,

    hoax:expect(receive
                  deliv_db:qfetch(scm_github_project_metadata, update_by_id, [ProjId, RepoOwner, RepoName]) -> [project]
                end),

    Result = scm_github_project_metadata:update_by_id(ProjId, RepoOwner, RepoName),
    ?assertEqual({ok, project}, Result),
    ?verifyAll.

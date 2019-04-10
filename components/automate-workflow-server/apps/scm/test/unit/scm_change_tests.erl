-module(scm_change_tests).

-include_lib("hoax/include/hoax.hrl").

-compile(export_all).

client_details_test_() ->
    hoax:fixture(?MODULE).

fetch_by_change_id_returns_metadata() ->
    ChangeId = <<"change-id">>,
    PRUrl = <<"http://bitbucket-pr">>,
    PRId = 1,
    ChangeMetaData = scm_change:fromlist([{pr_url, PRUrl}, {pr_id, PRId}]),

    hoax:mock(deliv_db,
              ?expect(qfetch,
                      ?withArgs([scm_change,
                                 fetch,
                                 [ChangeId]]),
                      ?andReturn([ChangeMetaData]))),

    ?assertEqual({ok, ChangeMetaData}, (scm_change:fetch_by_change_id(ChangeId))),
    ?verifyAll.

save_saves_and_returns_metadata() ->
    ChangeId = <<"change-id">>,
    PRUrl = <<"http://bitbucket-pr">>,
    PRId = 1,
    % full API response can be found at
    % https://developer.atlassian.com/static/rest/stash/3.11.3/stash-rest.html
    % if you search for "pull-request", click on the first link.
    ChangeMetaData = scm_change:fromlist([{change_id, ChangeId}, {pr_url, PRUrl}, {pr_id, PRId}]),

    hoax:mock(deliv_db,
              ?expect(insert,
                      ?withArgs([ChangeMetaData]),
                      ?andReturn([ChangeMetaData]))),

    ?assertEqual([ChangeMetaData], (scm_change:save(ChangeId, PRId, PRUrl))),
    ?verifyAll.

get_hal_returns_json_metadata_for_scm() ->
    ChangeId = <<"change-id">>,
    PullRequestUrl = <<"http://bitbucket-pr">>,
    PrId =  1,
    PullRequestTitle = <<"SCM PR Description">>,
    ChangeMetaData = scm_change:fromlist([{change_id, ChangeId}, {pr_url, PullRequestUrl}, {pr_id, PrId}]),
    HalMetaData = {[{<<"href">>, PullRequestUrl},
                    {<<"title">>, PullRequestTitle}]},
    MockScmModuleName = mock_scm_module,

    hoax:mock(deliv_db,
              ?expect(qfetch,
                      ?withArgs([scm_change,
                                 fetch,
                                 [ChangeId]]),
                      ?andReturn([ChangeMetaData]))),
    hoax:mock_behaviour(deliv_scm, MockScmModuleName,
                        ?expect(pull_request_description,
                                ?withArgs([PrId]),
                                ?andReturn(PullRequestTitle))),

    Result = scm_change:get_hal(ChangeId, MockScmModuleName),
    ?assertEqual(HalMetaData, Result),
    ?verifyAll.

get_hal_returns_empty_json_when_change_not_found() ->
    ChangeId = <<"change-id">>,
    HalMetaData = {[]},

    hoax:mock(deliv_db,
              ?expect(qfetch,
                      ?withArgs([scm_change,
                                 fetch,
                                 [ChangeId]]),
                      ?andReturn({error, not_found}))),

    Result = scm_change:get_hal(ChangeId, any_scm_module),
    ?assertEqual(HalMetaData, Result),
    ?verifyAll.

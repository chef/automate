-module(scm_bitbucket_rest_api_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_coordinates.hrl").
-include_lib("delivery/include/deliv_types.hrl").

-compile(export_all).

hoax_fixture_test_() ->
    hoax:fixture(?MODULE).

mock_pull_request_body(Patchset, SourceBranch, DestinationBranch, Metadata) ->
    Ent = <<"ent_name">>,
    Org = <<"org_name">>,
    Proj = <<"proj_name">>,
    ChangeId = <<"change_id">>,
    ChangeUrl = <<"https://mydelivery/e/", Ent/binary, "/#/organizations/",
                  Org/binary, "/projects/", Proj/binary, "/changes/", ChangeId/binary>>,

    Change = deliv_change:'#new'(),
    ChangeTitle = <<"Title">>,
    ChangeDescription = <<"Description">>,

    BitbucketProject = <<"project">>,
    BitbucketRepoName = <<"repo">>,

    hoax:mock(deliv_patchset,
              ?expect(getval,
                      ?withArgs([change_id, Patchset]),
                      ?andReturn(<<"change_id">>))),
    hoax:mock(deliv_change, [
              ?expect(fetch_by_id,
                      ?withArgs([<<"change_id">>]),
                      ?andReturn({ok, Change})),
              ?expect(getval,
                      ?withArgs([title, Change]),
                      ?andReturn(ChangeTitle)),
              ?expect(getval,
                      ?withArgs([description, Change]),
                      ?andReturn(ChangeDescription)),
              ?expect(scoping_names,
                      ?withArgs([ChangeId]),
                      ?andReturn([Ent, Org, Proj, <<"pipe">>]))]),
    hoax:mock(scm_bitbucket_project_metadata, [
              ?expect(getval,
                      ?withArgs([bitbucket_project, Metadata]),
                      ?andReturn(BitbucketProject)),
              ?expect(getval,
                      ?withArgs([repo_name, Metadata]),
                      ?andReturn(BitbucketRepoName))]),
    hoax:mock(deliv_web_utils,
              ?expect(make_web_url_for_change,
                      ?withArgs([Ent, Org, Proj, ChangeId]),
                      ?andReturn(ChangeUrl))),

    ExpectedDescription = <<"Description\n\nReady to merge? View this change in ",
                            "Chef Automate and click the Approve button:\n",
                            ChangeUrl/binary>>,
    {[{<<"title">>, ChangeTitle},
      {<<"description">>, ExpectedDescription},
      {<<"state">>, <<"OPEN">>},
      {<<"open">>, true},
      {<<"closed">>, false},
      {<<"fromRef">>,
        {[{<<"id">>, SourceBranch},
          {<<"repository">>,
            {[{<<"slug">>, BitbucketRepoName},
              {<<"name">>, null},
              {<<"project">>,
                {[{<<"key">>, BitbucketProject}]}}]}}]}},
      {<<"toRef">>,
        {[{<<"id">>, DestinationBranch},
          {<<"repository">>,
            {[{<<"slug">>, BitbucketRepoName},
              {<<"name">>, null},
              {<<"project">>,
                {[{<<"key">>, BitbucketProject}]}}]}}]}},
      {<<"locked">>, false},
      {<<"reviewers">>, []}]}.

mock_repo_url(BasicAuth, Metadata) ->
    RootApi = <<"http://bitbucket.internal">>,

    BitbucketProject = <<"project">>,
    BitbucketRepoName = <<"repo">>,

    hoax:mock(deliv_basic_auth_application,
              ?expect(getval,
                      ?withArgs([root_api_url, BasicAuth]),
                      ?andReturn(RootApi))
             ),

    hoax:mock(scm_bitbucket_project_metadata, [
              ?expect(getval,
                      ?withArgs([bitbucket_project, Metadata]),
                      ?andReturn(BitbucketProject)),
              ?expect(getval,
                      ?withArgs([repo_name, Metadata]),
                      ?andReturn(BitbucketRepoName))
                 ]),

    [RootApi, "/rest/api/1.0/projects/", BitbucketProject, "/repos/", BitbucketRepoName, "/pull-requests"].

mock_request_headers(BasicAuth) ->
    UserName = <<"username">>,
    Password = <<"password">>,

    hoax:mock(deliv_basic_auth_application, [
              ?expect(getval,
                      ?withArgs([user_id, BasicAuth]),
                      ?andReturn(UserName)),
              ?expect(getval,
                      ?withArgs([password, BasicAuth]),
                      ?andReturn(Password))
             ]),

    Encoded = base64:encode_to_string(<<UserName/binary,":",Password/binary>>),
    [{"Authorization","Basic " ++ Encoded}].

ensure_pull_request_given_http_201_success_returns_ok_hal() ->
    EntName = <<"TheFiggisAgency">>,
    Patchset = deliv_patchset:'#new'(),
    SourceBranch = <<"feature1">>,
    DestinationBranch = <<"master">>,
    Coords = #proj_coordinates{ent_name = EntName},
    Metadata = scm_bitbucket_project_metadata:'#new'(),
    BasicAuth = deliv_basic_auth_application:'#new'(),

    Url = mock_repo_url(BasicAuth, Metadata),
    Body = mock_pull_request_body(Patchset, SourceBranch, DestinationBranch, Metadata),
    Headers = mock_request_headers(BasicAuth),

    HttpResponseBody = <<"{\"links\": {\"self\": [{\"href\": \"http://link/to/pullrequest\"}]}}">>,

    hoax:mock(scm_bitbucket_project_metadata,
              ?expect(fetch_by_project_coords,
                      ?withArgs([Coords]),
                      ?andReturn({ok, Metadata}))),
    hoax:mock(scm_basic_auth,
              ?expect(load_basic_auth_credentials,
                      ?withArgs([EntName, <<"bitbucket">>]),
                      ?andReturn({ok, BasicAuth}))),
    hoax:mock(deliv_http,
              ?expect(req,
                      ?withArgs([post, Url, Body, Headers]),
                      ?andReturn({ok, 201, Headers, HttpResponseBody}))),

    ExpectedResponse = {ok, chef_json:decode(HttpResponseBody)},
    Result = scm_bitbucket_rest_api:ensure_pull_request(Patchset, SourceBranch,
                                                    DestinationBranch, Coords),
    ?assertEqual(ExpectedResponse, Result),
    ?verifyAll.

ensure_pull_request_given_http_409_error_returns_exists() ->
    EntName = <<"TheFiggisAgency">>,
    Patchset = deliv_patchset:'#new'(),
    SourceBranch = <<"feature1">>,
    DestinationBranch = <<"master">>,
    Coords = #proj_coordinates{ent_name = EntName},
    Metadata = scm_bitbucket_project_metadata:'#new'(),
    BasicAuth = deliv_basic_auth_application:'#new'(),

    Url = mock_repo_url(BasicAuth, Metadata),
    Body = mock_pull_request_body(Patchset, SourceBranch, DestinationBranch, Metadata),
    Headers = mock_request_headers(BasicAuth),

    HttpResponseBody =
      <<"{\"errors\": [
            {\"exceptionName\": \"com.atlassian.stash.pull.DuplicatePullRequestException\",
             \"existingPullRequest\": [
                 {\"links\":
                     {\"self\": [
                         {\"href\": \"http://link/to/pullrequest\"}
                     ]}
                 }
            ]}
        ]}">>,

    hoax:mock(scm_bitbucket_project_metadata,
              ?expect(fetch_by_project_coords,
                      ?withArgs([Coords]),
                      ?andReturn({ok, Metadata}))),
    hoax:mock(scm_basic_auth,
              ?expect(load_basic_auth_credentials,
                      ?withArgs([EntName, <<"bitbucket">>]),
                      ?andReturn({ok, BasicAuth}))),
    hoax:mock(deliv_http,
              ?expect(req,
                      ?withArgs([post, Url, Body, Headers]),
                      ?andReturn({ok, 409, Headers, HttpResponseBody}))),

    ExpectedResponse = {exists, <<"Bitbucket Pull Request: http://link/to/pullrequest">>},
    Result = scm_bitbucket_rest_api:ensure_pull_request(Patchset, SourceBranch,
                                                    DestinationBranch, Coords),
    ?assertEqual(ExpectedResponse, Result),
    ?verifyAll.

ensure_pull_request_given_http_error_non_409_returns_error() ->
    EntName = <<"TheFiggisAgency">>,
    Patchset = deliv_patchset:'#new'(),
    SourceBranch = <<"feature1">>,
    DestinationBranch = <<"master">>,
    Coords = #proj_coordinates{ent_name = EntName},
    Metadata = scm_bitbucket_project_metadata:'#new'(),
    BasicAuth = deliv_basic_auth_application:'#new'(),

    Url = mock_repo_url(BasicAuth, Metadata),
    Body = mock_pull_request_body(Patchset, SourceBranch, DestinationBranch, Metadata),
    Headers = mock_request_headers(BasicAuth),

    hoax:mock(scm_bitbucket_project_metadata,
              ?expect(fetch_by_project_coords,
                      ?withArgs([Coords]),
                      ?andReturn({ok, Metadata}))),
    hoax:mock(scm_basic_auth,
              ?expect(load_basic_auth_credentials,
                      ?withArgs([EntName, <<"bitbucket">>]),
                      ?andReturn({ok, BasicAuth}))),
    hoax:mock(deliv_http,
              ?expect(req,
                      ?withArgs([post, Url, Body, Headers]),
                      ?andReturn({ok, 400, Headers, <<"Body">>}))),

    ExpectedResponse = {error, <<"Failed to open a Pull Request in Bitbucket with response code 400.">>},
    Result = scm_bitbucket_rest_api:ensure_pull_request(Patchset, SourceBranch,
                                                    DestinationBranch, Coords),
    ?assertEqual(ExpectedResponse, Result),
    ?verifyAll.

ensure_pull_request_given_failed_http_request_returns_error() ->
    EntName = <<"TheFiggisAgency">>,
    Patchset = deliv_patchset:'#new'(),
    SourceBranch = <<"feature1">>,
    DestinationBranch = <<"master">>,
    Coords = #proj_coordinates{ent_name = EntName},
    Metadata = scm_bitbucket_project_metadata:'#new'(),
    BasicAuth = deliv_basic_auth_application:'#new'(),

    Url = mock_repo_url(BasicAuth, Metadata),
    Body = mock_pull_request_body(Patchset, SourceBranch, DestinationBranch, Metadata),
    Headers = mock_request_headers(BasicAuth),

    hoax:mock(scm_bitbucket_project_metadata,
              ?expect(fetch_by_project_coords,
                      ?withArgs([Coords]),
                      ?andReturn({ok, Metadata}))),
    hoax:mock(scm_basic_auth,
              ?expect(load_basic_auth_credentials,
                      ?withArgs([EntName, <<"bitbucket">>]),
                      ?andReturn({ok, BasicAuth}))),
    hoax:mock(deliv_http,
              ?expect(req,
                      ?withArgs([post, Url, Body, Headers]),
                      ?andReturn({error, <<"Why For?">>}))),

    ExpectedResponse = {error, <<"Failed to open a Pull Request in Bitbucket with error: Why For?">>},
    Result = scm_bitbucket_rest_api:ensure_pull_request(Patchset, SourceBranch,
                                                    DestinationBranch, Coords),
    ?assertEqual(ExpectedResponse, Result),
    ?verifyAll.

check_reachability_returns_ok_if_bitbucket_is_reachable_and_creds_work() ->
    Url = <<"http://bitbucket.ci">>,
    Username = <<"foo">>,
    Password = <<"bar">>,
    Body = [],
    Encoded = base64:encode_to_string(<<Username/binary, ":", Password/binary>>),
    Headers = [{"Authorization","Basic " ++ Encoded}],

    hoax:mock(deliv_http,
              ?expect(req,
                      ?withArgs([get, <<Url/binary, "/rest/api/1.0/projects">>, Body, Headers]),
                      ?andReturn({ok, 200, [], "Success"}))),

    ExpectedResponse = {ok, authenticated},
    ?assertEqual(ExpectedResponse, scm_bitbucket_rest_api:check_reachability(Url, Username, Password)),
    ?verifyAll.

check_reachability_returns_error_if_bitbucket_is_reachable_and_creds_fail() ->
    Url = <<"http://bitbucket.ci">>,
    Username = <<"foo">>,
    Password = <<"bar">>,
    Body = [],
    Encoded = base64:encode_to_string(<<Username/binary, ":", Password/binary>>),
    Headers = [{"Authorization","Basic " ++ Encoded}],

    hoax:mock(deliv_http,
              ?expect(req,
                      ?withArgs([get, <<Url/binary, "/rest/api/1.0/projects">>, Body, Headers]),
                      ?andReturn({ok, 401, [], "Unauthorized"}))),

    ExpectedResponse = {error, unauthorized},
    ?assertEqual(ExpectedResponse, scm_bitbucket_rest_api:check_reachability(Url, Username, Password)),
    ?verifyAll.

check_reachability_returns_error_if_bitbucket_is_unreachable() ->
    Url = <<"http://bitbucket.ci">>,
    Username = <<"foo">>,
    Password = <<"bar">>,
    Body = [],
    Encoded = base64:encode_to_string(<<Username/binary, ":", Password/binary>>),
    Headers = [{"Authorization","Basic " ++ Encoded}],

    hoax:mock(deliv_http,
              ?expect(req,
                      ?withArgs([get, <<Url/binary, "/rest/api/1.0/projects">>, Body, Headers]),
                      ?andReturn({error, <<"Oh no!">>}))),

    ExpectedResponse = {error, <<"Oh no!">>},
    ?assertEqual(ExpectedResponse, scm_bitbucket_rest_api:check_reachability(Url, Username, Password)),
    ?verifyAll.

check_reachability_returns_error_with_location_on_redirect() ->
    Url = <<"http://bitbucket.ci">>,
    Username = <<"foo">>,
    Password = <<"bar">>,
    Body = [],
    Encoded = base64:encode_to_string(<<Username/binary, ":", Password/binary>>),
    Headers = [{"Authorization","Basic " ++ Encoded}],
    Location = "https://bitbucket.ci",
    ResponseHeaders = [{"Location", Location}],

    hoax:mock(deliv_http,
              ?expect(req,
                      ?withArgs([get, <<Url/binary, "/rest/api/1.0/projects">>, Body, Headers]),
                      ?andReturn({ok, 301, ResponseHeaders, body_ignored}))),

    ExpectedResponse = {error, {redirect, Location}},
    ?assertEqual(ExpectedResponse, scm_bitbucket_rest_api:check_reachability(Url, Username, Password)),
    ?verifyAll.

check_reachability_returns_error_not_found_on_404() ->
    Url = <<"http://bitbucket.ci">>,
    Username = <<"foo">>,
    Password = <<"bar">>,
    Body = [],
    Encoded = base64:encode_to_string(<<Username/binary, ":", Password/binary>>),
    Headers = [{"Authorization","Basic " ++ Encoded}],

    hoax:mock(deliv_http,
              ?expect(req,
                      ?withArgs([get, <<Url/binary, "/rest/api/1.0/projects">>, Body, Headers]),
                      ?andReturn({ok, 404, headers_ignored, body_ignored}))),

    ExpectedResponse = {error, not_found},
    ?assertEqual(ExpectedResponse, scm_bitbucket_rest_api:check_reachability(Url, Username, Password)),
    ?verifyAll.

-module(scm_github_rest_api_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_coordinates.hrl").
-include_lib("delivery/include/deliv_types.hrl").
-include("../../src/scm_types.hrl").

-compile(export_all).

ensure_fixture_test_() ->
    hoax:fixture(?MODULE, "ensure_", setup, teardown).

check_reachability_fixture_test_() ->
    hoax:fixture(?MODULE, "check_reachability").

setup() ->
    EntName = <<"TheFiggisAgency">>,
    ChangeId = <<"change_id">>,
    ChangeTitle = <<"Title">>,
    ChangeDescription = <<"Description">>,
    SubmitterName = <<"Timothy">>,
    hoax:mock(deliv_change, [
              ?expect(fetch_by_id,
                      ?withArgs([<<"change_id">>]),
                      ?andReturn({ok, change})),
              ?expect(getval,
                      ?withArgs([title, change]),
                      ?andReturn(ChangeTitle)),
              ?expect(getval,
                      ?withArgs([description, change]),
                      ?andReturn(ChangeDescription)),
              ?expect(getval,
                      ?withArgs([submitted_by, change]),
                      ?andReturn(SubmitterName)),
              ?expect(getval,
                      ?withArgs([id, change]),
                      ?andReturn(ChangeId))]),
    hoax:mock(scm_basic_auth,
              ?expect(load_basic_auth_credentials,
                      ?withArgs([EntName, <<"github">>]),
                      ?andReturn({ok, basic_auth}))).

teardown(_) ->
    ok.

mock_pull_request_body(Patchset, SourceBranch, DestinationBranch, #proj_coordinates{ent_name = EntName, org_name = OrgName, proj_name = ProjName}, UserMessage) ->
    AutomateUrl = <<"http://delivery/approve-here">>,
    Description = <<UserMessage/binary, "Description",
                    "\n\n----\nReady to merge? [View this change](",
                    AutomateUrl/binary,
                    ") in Chef Automate and click the Approve button.">>,

    hoax:mock(deliv_patchset,
              ?expect(getval,
                      ?withArgs([change_id, Patchset]),
                      ?andReturn(<<"change_id">>))),
    hoax:mock(deliv_web_utils,
              ?expect(make_web_url_for_change,
                      ?withArgs([EntName, OrgName, ProjName, <<"change_id">>]),
                      ?andReturn(AutomateUrl))),

    {[{<<"title">>, <<"Title">>},
      {<<"head">>, SourceBranch},
      {<<"base">>, DestinationBranch},
      {<<"body">>, Description}]}.

mock_repo_url() ->
    RootApi = <<"http://github.internal">>,

    GitHubProject = <<"project">>,
    GitHubRepoName = <<"repo">>,

    hoax:mock(deliv_basic_auth_application,
              ?expect(getval,
                      ?withArgs([root_api_url, basic_auth]),
                      ?andReturn(RootApi))
             ),

    iolist_to_binary([RootApi, ":80/api/v3/repos/", GitHubProject, "/", GitHubRepoName, "/pulls"]).

mock_submitter(EntName, UserName) ->
    Description = <<"This Pull Request was opened by Chef Automate user Whoever\n\n">>,
    hoax:mock(deliv_user,
              ?expect(user_description_for_pr_comment,
                      ?withArgs([EntName, UserName]),
                      ?andReturn(Description))),
    Description.

mock_request_headers() ->
    Token = <<"token">>,

    hoax:mock(deliv_basic_auth_application,
              ?expect(getval,
                      ?withArgs([password, basic_auth]),
                      ?andReturn(Token))),
    headers(Token).

ensure_pull_request_with_user_puts_display_name_on_pr_user_description() ->
    EntName = <<"TheFiggisAgency">>,
    OrgName = <<"SeniorFigg">>,
    ProjName = <<"FetchFiggs">>,
    Patchset = deliv_patchset:'#new'(),
    SourceBranch = <<"feature1">>,
    DestinationBranch = <<"master">>,
    Coords = #proj_coordinates{ent_name = EntName, org_name = OrgName, proj_name = ProjName},
    GitHubProject = <<"project">>,
    GitHubRepoName = <<"repo">>,
    Metadata = #metadata_by_scm{scm_type = <<"github">>, repo_name = GitHubRepoName, repo_group = GitHubProject},
    Url = mock_repo_url(),
    UserName = <<"Timothy">>,
    UserDescription = mock_submitter(EntName, UserName),
    Body = mock_pull_request_body(Patchset, SourceBranch, DestinationBranch, Coords, UserDescription),
    Headers = mock_request_headers(),

    HttpResponseBody = <<"{\"links\": {\"self\": [{\"href\": \"http://link/to/pullrequest\"}]}}">>,

    hoax:mock(scm_bitbucket_project_metadata,
              ?expect(fetch_scm_metadata_by_coords,
                      ?withArgs([Coords]),
                      ?andReturn({ok, Metadata}))),
    hoax:mock(deliv_http,
              ?expect(req,
                      ?withArgs([post, Url, Body, Headers]),
                      ?andReturn({ok, 201, Headers, HttpResponseBody}))),

    ExpectedResponse = {ok, chef_json:decode(HttpResponseBody)},
    Result = scm_github_rest_api:ensure_pull_request(Patchset, SourceBranch,
                                                 DestinationBranch, Coords),
    ?assertEqual(ExpectedResponse, Result),
    ?verifyAll.


ensure_pull_request_given_http_201_success_returns_ok_hal() ->
    EntName = <<"TheFiggisAgency">>,
    OrgName = <<"SeniorFigg">>,
    ProjName = <<"FetchFiggs">>,
    Patchset = deliv_patchset:'#new'(),
    SourceBranch = <<"feature1">>,
    DestinationBranch = <<"master">>,
    Coords = #proj_coordinates{ent_name = EntName, org_name = OrgName, proj_name = ProjName},
    GitHubProject = <<"project">>,
    GitHubRepoName = <<"repo">>,
    Metadata = #metadata_by_scm{scm_type = <<"github">>, repo_name = GitHubRepoName, repo_group = GitHubProject},
    Url = mock_repo_url(),
    UserName = <<"Timothy">>,
    UserDescription = mock_submitter(EntName, UserName),
    Body = mock_pull_request_body(Patchset, SourceBranch, DestinationBranch, Coords, UserDescription),
    Headers = mock_request_headers(),

    HttpResponseBody = <<"{\"links\": {\"self\": [{\"href\": \"http://link/to/pullrequest\"}]}}">>,

    hoax:mock(scm_bitbucket_project_metadata,
              ?expect(fetch_scm_metadata_by_coords,
                      ?withArgs([Coords]),
                      ?andReturn({ok, Metadata}))),
    hoax:mock(deliv_http,
              ?expect(req,
                      ?withArgs([post, Url, Body, Headers]),
                      ?andReturn({ok, 201, Headers, HttpResponseBody}))),

    ExpectedResponse = {ok, chef_json:decode(HttpResponseBody)},
    Result = scm_github_rest_api:ensure_pull_request(Patchset, SourceBranch,
                                                 DestinationBranch, Coords),
    ?assertEqual(ExpectedResponse, Result),
    ?verifyAll.

ensure_pull_request_given_http_422_error_with_existing_scm_change_returns_exists_with_pr_url() ->
    EntName = <<"TheFiggisAgency">>,
    OrgName = <<"SeniorFigg">>,
    ProjName = <<"FetchFiggs">>,
    Patchset = deliv_patchset:'#new'(),
    SourceBranch = <<"feature1">>,
    DestinationBranch = <<"master">>,
    Coords = #proj_coordinates{ent_name = EntName, org_name = OrgName, proj_name = ProjName},
    GitHubProject = <<"project">>,
    GitHubRepoName = <<"repo">>,
    Metadata = #metadata_by_scm{scm_type = <<"github">>, repo_name = GitHubRepoName, repo_group = GitHubProject},
    Url = mock_repo_url(),
    UserName = <<"Timothy">>,
    UserDescription = mock_submitter(EntName, UserName),
    Body = mock_pull_request_body(Patchset, SourceBranch, DestinationBranch, Coords, UserDescription),
    Headers = mock_request_headers(),
    ChangeId = deliv_patchset:getval(change_id, Patchset),
    ChangeMetadata = scm_change:fromlist([{pr_url, <<"https://github-acceptance.shd.chef.co/dmarion/example1/pull/1">>},
                                          {change_id, ChangeId}]),

    HttpResponseBody = <<"{\"message\":\"Validation Failed\",
                           \"errors\":[
                             {\"resource\":\"PullRequest\",
                              \"code\":\"custom\",
                              \"message\":\"A pull request already exists for project:feature1.\"
                             }],
                           \"documentation_url\":\"https://developer.github.com/enterprise/2.5/v3/pulls/#create-a-pull-request\"
                          }">>,

    hoax:mock(scm_bitbucket_project_metadata,
              ?expect(fetch_scm_metadata_by_coords,
                      ?withArgs([Coords]),
                      ?andReturn({ok, Metadata}))),
    hoax:mock(scm_change, [
              ?expect(fetch_by_change_id,
                      ?withArgs([ChangeId]),
                      ?andReturn({ok, ChangeMetadata})),
            ?expect(getval,
                    ?withArgs([pr_url, ChangeMetadata]),
                    ?andReturn(<<"https://github-acceptance.shd.chef.co/dmarion/example1/pull/1">>))]),
    hoax:mock(deliv_http,
              ?expect(req,
                      ?withArgs([post, Url, Body, Headers]),
                      ?andReturn({ok, 422, Headers, HttpResponseBody}))),

    ExpectedResponse = {exists, <<"GitHub Pull Request: https://github-acceptance.shd.chef.co/dmarion/example1/pull/1">>},
    Result = scm_github_rest_api:ensure_pull_request(Patchset, SourceBranch,
                                                    DestinationBranch, Coords),
    ?assertEqual(ExpectedResponse, Result),
    ?verifyAll.

ensure_pull_request_given_http_422_error_returns_exists() ->
    EntName = <<"TheFiggisAgency">>,
    OrgName = <<"SeniorFigg">>,
    ProjName = <<"FetchFiggs">>,
    Patchset = deliv_patchset:'#new'(),
    SourceBranch = <<"feature1">>,
    DestinationBranch = <<"master">>,
    Coords = #proj_coordinates{ent_name = EntName, org_name = OrgName, proj_name = ProjName},
    GitHubProject = <<"project">>,
    GitHubRepoName = <<"repo">>,
    Metadata = #metadata_by_scm{scm_type = <<"github">>, repo_name = GitHubRepoName, repo_group = GitHubProject},

    Url = mock_repo_url(),
    UserName = <<"Timothy">>,
    UserDescription = mock_submitter(EntName, UserName),
    Body = mock_pull_request_body(Patchset, SourceBranch, DestinationBranch, Coords, UserDescription),
    Headers = mock_request_headers(),
    ChangeId = deliv_patchset:getval(change_id, Patchset),

    HttpResponseBody = <<"{\"message\":\"Validation Failed\",
                           \"errors\":[
                             {\"resource\":\"PullRequest\",
                              \"code\":\"custom\",
                              \"message\":\"A pull request already exists for project:feature1.\"
                             }],
                           \"documentation_url\":\"https://developer.github.com/enterprise/2.5/v3/pulls/#create-a-pull-request\"
                          }">>,

    hoax:mock(scm_bitbucket_project_metadata,
              ?expect(fetch_scm_metadata_by_coords,
                      ?withArgs([Coords]),
                      ?andReturn({ok, Metadata}))),
    hoax:mock(scm_change,
              ?expect(fetch_by_change_id,
                      ?withArgs([ChangeId]),
                      ?andReturn({error, sorry_no_scm_change}))),
    hoax:mock(deliv_http,
              ?expect(req,
                      ?withArgs([post, Url, Body, Headers]),
                      ?andReturn({ok, 422, Headers, HttpResponseBody}))),

    ExpectedResponse = {exists, <<"GitHub Pull Request: A pull request already exists for project:feature1.">>},
    Result = scm_github_rest_api:ensure_pull_request(Patchset, SourceBranch,
                                                    DestinationBranch, Coords),
    ?assertEqual(ExpectedResponse, Result),
    ?verifyAll.

ensure_pull_request_given_http_422_with_garbage_json_error_returns_exists_with_custom_message_with_pr_url() ->
    EntName = <<"TheFiggisAgency">>,
    OrgName = <<"SeniorFigg">>,
    ProjName = <<"FetchFiggs">>,
    Patchset = deliv_patchset:'#new'(),
    SourceBranch = <<"feature1">>,
    DestinationBranch = <<"master">>,
    Coords = #proj_coordinates{ent_name = EntName, org_name = OrgName, proj_name = ProjName},
    GitHubProject = <<"project">>,
    GitHubRepoName = <<"repo">>,
    Metadata = #metadata_by_scm{scm_type = <<"github">>, repo_name = GitHubRepoName, repo_group = GitHubProject},

    Url = mock_repo_url(),
    UserName = <<"Timothy">>,
    UserDescription = mock_submitter(EntName, UserName),
    Body = mock_pull_request_body(Patchset, SourceBranch, DestinationBranch, Coords, UserDescription),
    Headers = mock_request_headers(),
    ChangeId = deliv_patchset:getval(change_id, Patchset),
    ChangeMetadata = scm_change:fromlist([{pr_url, <<"https://github-acceptance.shd.chef.co/dmarion/example1/pull/1">>},
                                          {change_id, ChangeId}]),

    HttpResponseBody = <<"{\"garbage_message\":\"Something is burning!\",
                           \"garbage_documentation_url\":\"https://developer.github.com/enterprise/2.5/v3/pulls/#create-a-pull-request\"
                          }">>,

    hoax:mock(scm_bitbucket_project_metadata,
              ?expect(fetch_scm_metadata_by_coords,
                      ?withArgs([Coords]),
                      ?andReturn({ok, Metadata}))),
    hoax:mock(deliv_http,
              ?expect(req,
                      ?withArgs([post, Url, Body, Headers]),
                      ?andReturn({ok, 422, Headers, HttpResponseBody}))),
    hoax:mock(scm_change, [
              ?expect(fetch_by_change_id,
                      ?withArgs([ChangeId]),
                      ?andReturn({ok, ChangeMetadata})),
            ?expect(getval,
                    ?withArgs([pr_url, ChangeMetadata]),
                    ?andReturn(<<"https://github-acceptance.shd.chef.co/dmarion/example1/pull/1">>))]),

    ExpectedResponse = {exists, <<"GitHub Pull Request: https://github-acceptance.shd.chef.co/dmarion/example1/pull/1">>},
    Result = scm_github_rest_api:ensure_pull_request(Patchset, SourceBranch,
                                                    DestinationBranch, Coords),
    ?assertEqual(ExpectedResponse, Result),
    ?verifyAll.

ensure_pull_request_given_http_422_with_garbage_json_error_returns_exists_with_custom_message() ->
    EntName = <<"TheFiggisAgency">>,
    OrgName = <<"SeniorFigg">>,
    ProjName = <<"FetchFiggs">>,
    Patchset = deliv_patchset:'#new'(),
    SourceBranch = <<"feature1">>,
    DestinationBranch = <<"master">>,
    Coords = #proj_coordinates{ent_name = EntName, org_name = OrgName, proj_name = ProjName},
    GitHubProject = <<"project">>,
    GitHubRepoName = <<"repo">>,
    Metadata = #metadata_by_scm{scm_type = <<"github">>, repo_name = GitHubRepoName, repo_group = GitHubProject},

    Url = mock_repo_url(),
    UserName = <<"Timothy">>,
    UserDescription = mock_submitter(EntName, UserName),
    Body = mock_pull_request_body(Patchset, SourceBranch, DestinationBranch, Coords, UserDescription),
    Headers = mock_request_headers(),
    ChangeId = deliv_patchset:getval(change_id, Patchset),

    HttpResponseBody = <<"{\"garbage_message\":\"Something is burning!\",
                           \"garbage_documentation_url\":\"https://developer.github.com/enterprise/2.5/v3/pulls/#create-a-pull-request\"
                          }">>,

    hoax:mock(scm_bitbucket_project_metadata,
              ?expect(fetch_scm_metadata_by_coords,
                      ?withArgs([Coords]),
                      ?andReturn({ok, Metadata}))),
    hoax:mock(deliv_http,
              ?expect(req,
                      ?withArgs([post, Url, Body, Headers]),
                      ?andReturn({ok, 422, Headers, HttpResponseBody}))),
    hoax:mock(scm_change,
              ?expect(fetch_by_change_id,
                      ?withArgs([ChangeId]),
                      ?andReturn({error, nah}))),

    ExpectedResponse = {exists, <<"Failed to open a Pull Request in GitHub with response code 422.">>},
    Result = scm_github_rest_api:ensure_pull_request(Patchset, SourceBranch,
                                                    DestinationBranch, Coords),
    ?assertEqual(ExpectedResponse, Result),
    ?verifyAll.

ensure_pull_request_given_http_error_non_422_returns_error() ->
    EntName = <<"TheFiggisAgency">>,
    Patchset = deliv_patchset:'#new'(),
    OrgName = <<"SeniorFigg">>,
    ProjName = <<"FetchFiggs">>,
    SourceBranch = <<"feature1">>,
    DestinationBranch = <<"master">>,
    Coords = #proj_coordinates{ent_name = EntName, org_name = OrgName, proj_name = ProjName},
    GitHubProject = <<"project">>,
    GitHubRepoName = <<"repo">>,
    Metadata = #metadata_by_scm{scm_type = <<"github">>, repo_name = GitHubRepoName, repo_group = GitHubProject},

    Url = mock_repo_url(),
    UserName = <<"Timothy">>,
    UserDescription = mock_submitter(EntName, UserName),
    Body = mock_pull_request_body(Patchset, SourceBranch, DestinationBranch, Coords, UserDescription),
    Headers = mock_request_headers(),

    hoax:mock(scm_bitbucket_project_metadata,
              ?expect(fetch_scm_metadata_by_coords,
                      ?withArgs([Coords]),
                      ?andReturn({ok, Metadata}))),
    hoax:mock(deliv_http,
              ?expect(req,
                      ?withArgs([post, Url, Body, Headers]),
                      ?andReturn({ok, 400, Headers, <<"Body">>}))),

    ExpectedResponse = {error, <<"Failed to open a Pull Request in GitHub with response code 400.">>},
    Result = scm_github_rest_api:ensure_pull_request(Patchset, SourceBranch,
                                                    DestinationBranch, Coords),
    ?assertEqual(ExpectedResponse, Result),
    ?verifyAll.

ensure_pull_request_given_failed_http_request_returns_error() ->
    EntName = <<"TheFiggisAgency">>,
    OrgName = <<"SeniorFigg">>,
    ProjName = <<"FetchFiggs">>,
    Patchset = deliv_patchset:'#new'(),
    SourceBranch = <<"feature1">>,
    DestinationBranch = <<"master">>,
    Coords = #proj_coordinates{ent_name = EntName, org_name = OrgName, proj_name = ProjName},
    GitHubProject = <<"project">>,
    GitHubRepoName = <<"repo">>,
    Metadata = #metadata_by_scm{scm_type = <<"github">>, repo_name = GitHubRepoName, repo_group = GitHubProject},

    Url = mock_repo_url(),
    UserName = <<"Timothy">>,
    UserDescription = mock_submitter(EntName, UserName),
    Body = mock_pull_request_body(Patchset, SourceBranch, DestinationBranch, Coords, UserDescription),
    Headers = mock_request_headers(),

    hoax:mock(scm_bitbucket_project_metadata,
              ?expect(fetch_scm_metadata_by_coords,
                      ?withArgs([Coords]),
                      ?andReturn({ok, Metadata}))),
    hoax:mock(deliv_http,
              ?expect(req,
                      ?withArgs([post, Url, Body, Headers]),
                      ?andReturn({error, <<"Why For?">>}))),

    ExpectedResponse = {error, <<"Failed to open a Pull Request in GitHub with error: Why For?">>},
    Result = scm_github_rest_api:ensure_pull_request(Patchset, SourceBranch,
                                                    DestinationBranch, Coords),
    ?assertEqual(ExpectedResponse, Result),
    ?verifyAll.

check_reachability_forwards_error_if_connection_fails() ->
    Url = <<"https://my-github.com">>,
    Token = <<"1234abcd">>,
    RequestBody = [],
    Headers =  headers(Token),
    GitHubUrl = <<"https://my-github.com:443/api/v3/">>,
    Error = {error, {conn_failed, {error, nxdomain}}},

    hoax:expect(receive
                    deliv_http:req(get, GitHubUrl, RequestBody, Headers) -> Error
                end),

    Result = scm_github_rest_api:check_reachability(Url, Token),
    ?assertEqual(Error, Result),
    ?verifyAll.

check_reachability_returns_error_with_location_on_redirect() ->
    Url = <<"http://github.com">>,
    Token = <<"1234abcd">>,
    RequestBody = [],
    Headers =  headers(Token),
    GitHubUrl = <<"http://api.github.com:80/">>,
    Location = "https://github.com",
    ResponseHeaders = [{"Location", Location}],

    hoax:expect(receive
                    deliv_http:req(get, GitHubUrl, RequestBody, Headers) ->
                        {ok, 301, ResponseHeaders, body_ignored}
                end),

    Result = scm_github_rest_api:check_reachability(Url, Token),
    ?assertEqual({error, {redirect, Location}}, Result),
    ?verifyAll.

check_reachability_returns_error_not_found_on_404() ->
    Url = <<"http://github.com">>,
    Token = <<"1234abcd">>,
    RequestBody = [],
    Headers =  headers(Token),
    GitHubUrl = <<"http://api.github.com:80/">>,

    hoax:expect(receive
                    deliv_http:req(get, GitHubUrl, RequestBody, Headers) ->
                        {ok, 404, headers_ignored, body_ignored}
                end),

    Result = scm_github_rest_api:check_reachability(Url, Token),
    ?assertEqual({error, not_found}, Result),
    ?verifyAll.

check_reachability_returns_error_bad_request_if_github_returns_a_400() ->
    Url = <<"https://my-github.com">>,
    Token = <<"1234abcd">>,
    RequestBody = [],
    Headers =  headers(Token),
    GitHubUrl = <<"https://my-github.com:443/api/v3/">>,

    hoax:expect(receive
                    deliv_http:req(get, GitHubUrl, RequestBody, Headers) ->  {ok, 400, ignored, ignored}
                end),

    Result = scm_github_rest_api:check_reachability(Url, Token),
    ?assertEqual({error, bad_request}, Result),
    ?verifyAll.

check_reachability_returns_error_unauthorized_if_github_returns_a_401() ->
    Url = <<"https://my-github.com">>,
    Token = <<"1234abcd">>,
    RequestBody = [],
    Headers =  headers(Token),
    GitHubUrl = <<"https://my-github.com:443/api/v3/">>,

    hoax:expect(receive
                    deliv_http:req(get, GitHubUrl, RequestBody, Headers) ->  {ok, 401, ignored, ignored}
                end),

    Result = scm_github_rest_api:check_reachability(Url, Token),
    ?assertEqual({error, unauthorized}, Result),
    ?verifyAll.

check_reachability_returns_ok_authorized_if_github_returns_a_200() ->
    Url = <<"https://my-github.com">>,
    Token = <<"1234abcd">>,
    RequestBody = [],
    Headers =  headers(Token),
    GitHubUrl = <<"https://my-github.com:443/api/v3/">>,

    hoax:expect(receive
                    deliv_http:req(get, GitHubUrl, RequestBody, Headers) ->  {ok, 200, ignored, ignored}
                end),

    Result = scm_github_rest_api:check_reachability(Url, Token),
    ?assertEqual({ok, authenticated}, Result),
    ?verifyAll.

check_reachability_with_github_com_checks_api_github_com_() ->
    ConfiguredUrl = <<"https://github.com/">>,
    ExpectedUrl = <<"https://api.github.com:443/">>,
    Token = <<"123123">>,
    Headers =  headers(Token),

    hoax:expect(receive
                    deliv_http:req(get, ExpectedUrl, [], Headers) -> {ok, 200, ignored, ignored}
                end),

    Result = scm_github_rest_api:check_reachability(ConfiguredUrl, Token),
    ?assertEqual({ok, authenticated}, Result),
    ?verifyAll.

headers(Token) ->
    [{<<"Authorization">>, <<"Token ", Token/binary>>},
     {<<"User-Agent">>, <<"Chef-Automate-API">>}].

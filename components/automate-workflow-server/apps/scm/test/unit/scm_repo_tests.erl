-module(scm_repo_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_coordinates.hrl").
-include("../../src/scm_types.hrl").

-compile(export_all).

hoax_fixture_test_() ->
    hoax:fixture(?MODULE).

url_when_bitbucket_returns_bitbucket_git_url() ->
    EntName = <<"testent">>,
    OrgName = <<"orgname">>,
    ProjName = <<"projname">>,
    Coords = #proj_coordinates{ent_name = EntName, org_name = OrgName, proj_name = ProjName},
    BasicAuth = deliv_basic_auth_application:'#new'(),
    UserName = <<"Username">>,
    Password = <<";foo">>,
    RootApi = <<"http://example.com:8080">>,
    BitbucketMetadata = #metadata_by_scm{scm_type = <<"bitbucket">>,
                                         repo_name = <<"repo">>,
                                         repo_group = <<"project">>},

    hoax:expect(
      receive
          scm_bitbucket_project_metadata:fetch_scm_metadata_by_coords(Coords) -> {ok, BitbucketMetadata};
          scm_basic_auth:load_basic_auth_credentials(EntName, <<"bitbucket">>) -> {ok, BasicAuth};
          deliv_basic_auth_application:getval(user_id, BasicAuth) -> UserName;
          deliv_basic_auth_application:getval(password, BasicAuth) -> Password;
          deliv_basic_auth_application:getval(root_api_url, BasicAuth) -> RootApi
      end),


    {ok, ActualUrl} = scm_repo:url(Coords),

    ExpectedUrl = <<"http://Username:%3bfoo@example.com:8080/scm/project/repo.git">>,
    ?assertEqual(ExpectedUrl, ActualUrl),
    ?verifyAll.

url_when_bitbucket_with_path_after_port_returns_bitbucket_git_url_with_path() ->
    EntName = <<"testent">>,
    OrgName = <<"orgname">>,
    ProjName = <<"projname">>,
    Coords = #proj_coordinates{ent_name = EntName, org_name = OrgName, proj_name = ProjName},
    BasicAuth = deliv_basic_auth_application:'#new'(),
    UserName = <<"Username">>,
    Password = <<";foo">>,
    RootApi = <<"http://example.com:8080/stash">>,
    BitbucketMetadata = #metadata_by_scm{scm_type = <<"bitbucket">>,
                                         repo_name = <<"repo">>,
                                         repo_group = <<"project">>},

    hoax:expect(
      receive
          scm_bitbucket_project_metadata:fetch_scm_metadata_by_coords(Coords) -> {ok, BitbucketMetadata};
          scm_basic_auth:load_basic_auth_credentials(EntName, <<"bitbucket">>) -> {ok, BasicAuth};
          deliv_basic_auth_application:getval(user_id, BasicAuth) -> UserName;
          deliv_basic_auth_application:getval(password, BasicAuth) -> Password;
          deliv_basic_auth_application:getval(root_api_url, BasicAuth) -> RootApi
      end),

    {ok, ActualUrl} = scm_repo:url(Coords),

    ExpectedUrl = <<"http://Username:%3bfoo@example.com:8080/stash/scm/project/repo.git">>,
    ?assertEqual(ExpectedUrl, ActualUrl),
    ?verifyAll.

url_when_bitbucket_with_email_as_username_returns_bitbucket_git_url_with_encoded_usrname() ->
    EntName = <<"testent">>,
    OrgName = <<"orgname">>,
    ProjName = <<"projname">>,
    Coords = #proj_coordinates{ent_name = EntName, org_name = OrgName, proj_name = ProjName},
    BasicAuth = deliv_basic_auth_application:'#new'(),
    UserName = <<"user@bigcorp.com">>,
    Password = <<";foo">>,
    RootApi = <<"http://example.com:8080/stash">>,
    BitbucketMetadata = #metadata_by_scm{scm_type = <<"bitbucket">>,
                                         repo_name = <<"repo">>,
                                         repo_group = <<"project">>},

    hoax:expect(
      receive
          scm_bitbucket_project_metadata:fetch_scm_metadata_by_coords(Coords) -> {ok, BitbucketMetadata};
          scm_basic_auth:load_basic_auth_credentials(EntName, <<"bitbucket">>) -> {ok, BasicAuth};
          deliv_basic_auth_application:getval(user_id, BasicAuth) -> UserName;
          deliv_basic_auth_application:getval(password, BasicAuth) -> Password;
          deliv_basic_auth_application:getval(root_api_url, BasicAuth) -> RootApi
      end),

    {ok, ActualUrl} = scm_repo:url(Coords),

    ExpectedUrl = <<"http://user%40bigcorp.com:%3bfoo@example.com:8080/stash/scm/project/repo.git">>,
    ?assertEqual(ExpectedUrl, ActualUrl),
    ?verifyAll.

url_when_metadata_lookup_returns_error_return_that_error() ->
        EntName = <<"testent">>,
        OrgName = <<"orgname">>,
        ProjName = <<"projname">>,
        Coords = #proj_coordinates{ent_name = EntName, org_name = OrgName, proj_name = ProjName},

        hoax:expect(
          receive
              scm_bitbucket_project_metadata:fetch_scm_metadata_by_coords(Coords) -> {error, other_failure}
          end),

        ActualResult = scm_repo:url(Coords),

        ?assertEqual({error, other_failure}, ActualResult),
        ?verifyAll.

url_when_github_returns_github_git_url() ->
    EntName = <<"testent">>,
    OrgName = <<"orgname">>,
    ProjName = <<"projname">>,
    Coords = #proj_coordinates{ent_name = EntName, org_name = OrgName, proj_name = ProjName},
    BasicAuth = deliv_basic_auth_application:'#new'(),
    UserName = <<"automate">>,
    Token = <<"thisIsAToken">>,
    RootApi = <<"http://example.com:8080">>,
    GithubMetadata = #metadata_by_scm{scm_type = <<"github">>,
                                         repo_name = <<"github-repo">>,
                                         repo_group = <<"github-owner">>},

    hoax:expect(
      receive
          scm_bitbucket_project_metadata:fetch_scm_metadata_by_coords(Coords) -> {ok, GithubMetadata};
          scm_basic_auth:load_basic_auth_credentials(EntName, <<"github">>) -> {ok, BasicAuth};
          deliv_basic_auth_application:getval(user_id, BasicAuth) -> UserName;
          deliv_basic_auth_application:getval(password, BasicAuth) -> Token;
          deliv_basic_auth_application:getval(root_api_url, BasicAuth) -> RootApi
      end),

    {ok, ActualUrl} = scm_repo:url(Coords),

    ExpectedUrl = <<"http://thisIsAToken:x-oauth-basic@example.com:8080/github-owner/github-repo.git">>,
    ?assertEqual(ExpectedUrl, ActualUrl),
    ?verifyAll.

url_when_basic_auth_creds_query_fails_returns_error() ->
    Metadata = #metadata_by_scm{scm_type = <<"some_scm">>},
    Coords = #proj_coordinates{ent_name = <<"testent">>},

    hoax:expect(
      receive
          scm_bitbucket_project_metadata:fetch_scm_metadata_by_coords(Coords) -> {ok, Metadata};
          scm_basic_auth:load_basic_auth_credentials(<<"testent">>, <<"some_scm">>) ->
              {error, basic_auth_lookup_error}
      end),

    ActualResult = scm_repo:url(Coords),

    ?assertEqual({error, basic_auth_lookup_error}, ActualResult),
    ?verifyAll.

url_when_github_with_path_after_port_returns_github_git_url_with_path() ->
    EntName = <<"testent">>,
    OrgName = <<"orgname">>,
    ProjName = <<"projname">>,
    Coords = #proj_coordinates{ent_name = EntName, org_name = OrgName, proj_name = ProjName},
    BasicAuth = deliv_basic_auth_application:'#new'(),
    UserName = <<"automate">>,
    Token = <<"thisIsAToken">>,
    RootApi = <<"http://example.com:8080/github">>,
    GithubMetadata = #metadata_by_scm{scm_type = <<"github">>,
                                         repo_name = <<"github-repo">>,
                                         repo_group = <<"github-owner">>},

    hoax:expect(
      receive
          scm_bitbucket_project_metadata:fetch_scm_metadata_by_coords(Coords) -> {ok, GithubMetadata};
          scm_basic_auth:load_basic_auth_credentials(EntName, <<"github">>) -> {ok, BasicAuth};
          deliv_basic_auth_application:getval(user_id, BasicAuth) -> UserName;
          deliv_basic_auth_application:getval(password, BasicAuth) -> Token;
          deliv_basic_auth_application:getval(root_api_url, BasicAuth) -> RootApi
      end),

    {ok, ActualUrl} = scm_repo:url(Coords),

    ExpectedUrl = <<"http://thisIsAToken:x-oauth-basic@example.com:8080/github/github-owner/github-repo.git">>,
    ?assertEqual(ExpectedUrl, ActualUrl),
    ?verifyAll.

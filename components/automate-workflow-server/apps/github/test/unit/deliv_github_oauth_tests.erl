-module(deliv_github_oauth_tests).

-include_lib("hoax/include/hoax.hrl").

-compile(export_all).

all_test_() ->
    hoax:fixture(?MODULE).

-define(NAME, <<"github">>).
-define(ROOT_URL, <<"https://github.com">>).
-define(ROOT_API_URL, <<"https://api.github.com">>).
-define(CLIENT_ID, <<"my client id">>).
-define(CLIENT_SECRET, <<"swordfish">>).

access_token_url_returns_fetch_token_url() ->
    Code = ct_github:example_code(),
    Application = deliv_oauth_application:fromlist([{name, ?NAME},
                                                    {root_url, ?ROOT_URL},
                                                    {root_api_url, ?ROOT_API_URL},
                                                    {client_id, ?CLIENT_ID},
                                                    {client_secret, ?CLIENT_SECRET}]),

    Url = [?ROOT_URL,
           "/login/oauth/access_token",
           "?client_id=", ?CLIENT_ID,
           "&client_secret=", ?CLIENT_SECRET,
           "&code=", Code],
    ?assertEqual(Url, deliv_github_oauth:access_token_url(Code, Application)).


authorize_url_returns_token_request_url() ->
    StateParam = chef_utils:random_string(64),
    Application = deliv_oauth_application:fromlist([{name, ?NAME},
                                                    {root_url, ?ROOT_URL},
                                                    {root_api_url, ?ROOT_API_URL},
                                                    {client_id, ?CLIENT_ID},
                                                    {client_secret, ?CLIENT_SECRET}]),

    Url = chef_utils:iodata_to_str([?ROOT_URL,
                                     "/login/oauth/authorize",
                                     "?client_id=", ?CLIENT_ID,
                                     "&scope=", deliv_web_utils:encode_url(<<"admin:repo_hook,repo,write:public_key">>),
                                     "&state=", StateParam]),

    ?assertEqual(Url, deliv_github_oauth:authorize_url(StateParam, Application)).

fetch_token_happy_path_returns_token() ->
    Code = ct_github:example_code(),
    Application = deliv_oauth_application:fromlist([{name, ?NAME},
                                                    {root_url, ?ROOT_URL},
                                                    {root_api_url, ?ROOT_API_URL},
                                                    {client_id, ?CLIENT_ID},
                                                    {client_secret, ?CLIENT_SECRET}]),
    Url = [?ROOT_URL,
           "/login/oauth/access_token",
           "?client_id=", ?CLIENT_ID,
           "&client_secret=", ?CLIENT_SECRET,
           "&code=", Code],
    Token = ct_github:example_token(),
    ResponseFile = app_test_helpers:project_path(?MODULE, "test/ct/wiremock/__files/github_access_token_response.json"),
    {ok, SuccessRespBody} = file:read_file(ResponseFile),
    hoax:mock(deliv_http,
              ?expect(req,
                      ?withArgs([post, Url, <<>>, [{"Accept", <<"application/json">>}]]),
                      ?andReturn({ok, 200, [], SuccessRespBody}))),
    ?assertEqual({ok, Token},
                 deliv_github_oauth:fetch_token(Code, Application)),
    ?verifyAll.

fetch_app_by_enterprise_returns_oauth_app() ->
    OauthApp = deliv_oauth_application:'#new'(),
    Enterprise = deliv_enterprise:'#new'(),
    EntName = <<"EntName">>,
    EntId = 5,

    hoax:mock(deliv_enterprise, [
                              ?expect(fetch,
                                      ?withArgs([EntName]),
                                      ?andReturn({ok, Enterprise})),
                              ?expect(getval,
                                      ?withArgs([id, Enterprise]),
                                      ?andReturn(EntId))
                             ]),
    hoax:mock(deliv_oauth_application,
              ?expect(fetch_by_token_details,
                      ?withArgs([enterprise, EntId, deliv_github_oauth]),
                      ?andReturn({ok, OauthApp}))),
    ?assertEqual({ok, OauthApp}, deliv_github_oauth:fetch_app_by_enterprise(EntName)),
    ?verifyAll.

-module(ct_github).

-export([
         new_app_and_token/4,
         delete_github_app/0,
         load_raw_payload_from_disk/1,
         load_payload_from_disk/1,
         example_code/0,
         example_token/0,
         root_url/0,
         root_api_url/0,
         client_id/0,
         client_secret/0
        ]).

-include_lib("delivery/include/deliv_types.hrl").

-include_lib("hoax/include/hoax.hrl").

-define(DELIV_TEST_GITHUB_ROOT_URL, "https://localhost").
%% for github enterprise, the pattern for API url is <root_url>/api/v3.
%% for github.com, API url is https://api.github.com.
-define(DELIV_TEST_GITHUB_API_URL, "https://localhost/api/v3").
%% the lengths, at least, are the correct ones; probably not that relevant, but
%% still
-define(DELIV_TEST_GITHUB_APP_ID, <<"aaaaaaaaaaaaaaaaaaaa">>).
-define(DELIV_TEST_GITHUB_APP_SECRET, <<"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb">>).
-define(DELIV_TEST_GITHUB_CODE, <<"cccccccccccccccccccc">>).
-define(DELIV_TEST_GITHUB_TOKEN, <<"dddddddddddddddddddddddddddddddddddddddd">>).

-spec new_app_and_token(binary(), binary(), non_neg_integer(), binary()) -> {d_oauth_token(), d_oauth_application()}.
new_app_and_token(GithubOrg, GithubRepo, ProjId, EntId) ->
    deliv_project_github_metadata:upsert(ProjId, GithubOrg, GithubRepo, <<"unused">>),
    App = new_app(),
    TokenRec = new_token(EntId),
    {TokenRec, App}.

-spec new_app() -> d_oauth_application().
new_app() ->
    deliv_oauth_application:delete(<<"github">>),
    [Application] = deliv_oauth_application:insert(<<"github">>,
                                                   <<"deliv_github_oauth">>,
                                                   ?DELIV_TEST_GITHUB_ROOT_URL,
                                                   ?DELIV_TEST_GITHUB_API_URL,
                                                   ?DELIV_TEST_GITHUB_APP_ID,
                                                   ?DELIV_TEST_GITHUB_APP_SECRET),
                                                   Application.

-spec new_token(non_neg_integer()) -> d_oauth_token().
new_token(EntId) ->
    {ok, TokenRec} = deliv_oauth_token:initialize(<<"github">>, enterprise, EntId),
    {ok, TokenRecWithToken} = deliv_oauth_token:save_token(?DELIV_TEST_GITHUB_TOKEN, TokenRec),
    TokenRecWithToken.

-spec delete_github_app() -> ok | {error, atom()}.
delete_github_app() ->
    deliv_oauth_application:delete(<<"github">>).

-spec load_raw_payload_from_disk(binary()) -> binary().
load_raw_payload_from_disk(File) ->
    GithubPayloadsDir = app_test_helpers:project_path(?MODULE, "test/ct/github_payloads"),
    PullRequestPayload = filename:join(GithubPayloadsDir, File),
    {ok, Json} = file:read_file(PullRequestPayload),
    Json.

-spec load_payload_from_disk(binary()) -> json().
load_payload_from_disk(File) ->
    chef_json:decode(load_raw_payload_from_disk(File)).

%% @doc An example github code (a token to fetch a token!)
-spec example_code() -> binary().
example_code() ->
    ?DELIV_TEST_GITHUB_CODE.

%% @doc An example github token
-spec example_token() -> binary().
example_token() ->
    ?DELIV_TEST_GITHUB_TOKEN.

%% @doc The wiremock value for the root url
root_url() ->
    ?DELIV_TEST_GITHUB_ROOT_URL.

%% @doc The wiremock value for the root api url
root_api_url() ->
    ?DELIV_TEST_GITHUB_API_URL.

%% @doc The stubbed client_id
client_id() ->
    ?DELIV_TEST_GITHUB_APP_ID.

%% @doc The stubbed client secret
client_secret() ->
    ?DELIV_TEST_GITHUB_APP_SECRET.

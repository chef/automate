-module(ct_oauth_app).

-export([new/0, delete/0]).

-include_lib("delivery/include/deliv_types.hrl").

-define(DELIV_TEST_GITHUB_ROOT_URL, ct_wiremock:url()).
%% for github enterprise, the pattern for API url is <root_url>/api/v3.
%% for github.com, API url is https://api.github.com.
-define(DELIV_TEST_GITHUB_API_URL, ct_wiremock:url() ++ "/api/v3").
%% the lengths, at least, are the correct ones; probably not that relevant, but
%% still
-define(DELIV_TEST_GITHUB_APP_ID, <<"aaaaaaaaaaaaaaaaaaaa">>).
-define(DELIV_TEST_GITHUB_APP_SECRET, <<"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb">>).

-spec new() -> d_oauth_application().
new() ->
    deliv_oauth_application:delete(<<"github">>),
    [Application] = deliv_oauth_application:insert(<<"github">>,
                                                   <<"deliv_github_oauth">>,
                                                   ?DELIV_TEST_GITHUB_ROOT_URL,
                                                   ?DELIV_TEST_GITHUB_API_URL,
                                                   ?DELIV_TEST_GITHUB_APP_ID,
                                                   ?DELIV_TEST_GITHUB_APP_SECRET),
                                                   Application.

-spec delete() -> ok | {error, atom()}.
delete() ->
    deliv_oauth_application:delete(<<"github">>).


-module(deliv_oauth_application_tests).

-include_lib("hoax/include/hoax.hrl").

-compile(export_all).

list_oauth_applications_fixture_test_() ->
    hoax:fixture(?MODULE, "list_oauth_applications").

github_record() ->
    deliv_oauth_application:fromlist(
           [{id, 1},
           {module, <<"github">>},
           {name, <<"deliv_github_oauth">>},
           {root_url, <<"https://github.com">>},
           {root_api_url, <<"https://github.com/api/v3">>},
           {client_id, <<"COOLEST_CLIENT_ID">>},
           {client_secret, <<"HAPPY_SECRET">>}]
     ).

github_enterprise_record() ->
    deliv_oauth_application:fromlist(
           [{id, 2},
           {module, <<"github-enterprise">>},
           {name, <<"deliv_github_oauth">>},
           {root_url, <<"https://my-github.enterprise.com">>},
           {root_api_url, <<"https://my-github.enterprise.com/api/v3">>},
           {client_id, <<"IM_THE_CLIENT_ID">>},
           {client_secret, <<"AND_IM_ITS_SECRET">>}]
     ).

new_custom_application_record() ->
    deliv_oauth_application:fromlist(
           [{id, 3},
           {module, <<"custom-application">>},
           {name, <<"deliv_custom_oauth">>},
           {root_url, <<"https://custom-application.com">>},
           {root_api_url, <<"https://custom-application.com/api/v3">>},
           {client_id, <<"CUSTOM_CLIENT_ID">>},
           {client_secret, <<"CUSTOM_SECRET">>}]
     ).

list_oauth_applications_and_return_empty() ->
    EmptyRecord = [],
    hoax:mock(deliv_db,
              ?expect(qfetch,
                      ?withArgs([deliv_oauth_application, fetch_oauth_applications, []]),
                      ?andReturn(EmptyRecord))),
    ?assertEqual({ok, empty_list}, deliv_oauth_application:list()),
    ?verifyAll.

list_oauth_applications_and_return_github_application() ->
    hoax:mock(deliv_db,
              ?expect(qfetch,
                      ?withArgs([deliv_oauth_application, fetch_oauth_applications, []]),
                      ?andReturn([github_record()]))),
    ?assertEqual({ok, [github_record()]}, deliv_oauth_application:list()),
    ?verifyAll.

list_oauth_applications_and_return_github_enterprise_application() ->
    hoax:mock(deliv_db,
              ?expect(qfetch,
                      ?withArgs([deliv_oauth_application, fetch_oauth_applications, []]),
                      ?andReturn([github_enterprise_record()]))),
    ?assertEqual({ok, [github_enterprise_record()]}, deliv_oauth_application:list()),
    ?verifyAll.

list_oauth_applications_and_return_any_other_application_that_might_exist() ->
    hoax:mock(deliv_db,
              ?expect(qfetch,
                      ?withArgs([deliv_oauth_application, fetch_oauth_applications, []]),
                      ?andReturn([new_custom_application_record()]))),
    ?assertEqual({ok, [new_custom_application_record()]}, deliv_oauth_application:list()),
    ?verifyAll.

list_oauth_applications_and_return_github_and_github_enterprise_application() ->
    MultiAppList = [github_record(), github_enterprise_record(), new_custom_application_record()],
    hoax:mock(deliv_db,
              ?expect(qfetch,
                      ?withArgs([deliv_oauth_application, fetch_oauth_applications, []]),
                      ?andReturn(MultiAppList))),
    ?assertEqual({ok, MultiAppList}, deliv_oauth_application:list()),
    ?verifyAll.

list_oauth_applications_and_return_error() ->
    Why = "Because we have to test you my friend.",
    ErrRecord = {error, Why},
    hoax:mock(deliv_db,
              ?expect(qfetch,
                      ?withArgs([deliv_oauth_application, fetch_oauth_applications, []]),
                      ?andReturn(ErrRecord))),
    ?assertEqual(ErrRecord, deliv_oauth_application:list()),
    ?verifyAll.


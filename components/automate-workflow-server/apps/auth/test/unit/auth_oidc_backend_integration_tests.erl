-module(auth_oidc_backend_integration_tests).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

auth_oidc_backend_integration_test_() ->
    [eunit_sugar:fixture(?MODULE, "start_", setup, teardown),
     eunit_sugar:fixture(?MODULE, "associate_access_code", setup_code, teardown_code)
    ].

setup() ->
    application:set_env(auth, oidc, [[{client_id, <<"some-id-1">>},
                                      {client_secret, <<"some-secret-1">>},
                                      {client_redirect_uri, <<"https://some/callback1">>}
                                     ],
                                     [{client_id, <<"some-id-2">>},
                                      {client_secret, <<"some-secret-2">>},
                                      {client_redirect_uri, <<"https://some/callback2">>}
                                     ]
                                    ]),
    auth_oidc_backend:start().

teardown(_) ->
    auth_oidc_backend:stop(),
    application:unset_env(auth, oidc).

setup_code() ->
    application:set_env(oauth2, code_grant, [{expiry_time, 0}]), % "instant expiry"
    auth_oidc_backend:start().

teardown_code(_) ->
    auth_oidc_backend:stop(),
    application:unset_env(oauth2, code_grant).

start_adds_list_of_oidc_clients_that_can_be_authenticated() ->
    ?assertMatch({ok, _}, auth_oidc_backend:authenticate_client({<<"some-id-1">>, <<"some-secret-1">>}, app_ctx)),
    ?assertMatch({ok, _}, auth_oidc_backend:authenticate_client({<<"some-id-2">>, <<"some-secret-2">>}, app_ctx)).

start_adds_list_of_oidc_clients_whose_redirection_uri_can_be_retrieved() ->
    Actual1 = auth_oidc_backend:get_redirection_uri(<<"some-id-1">>, app_ctx),
    ?assertEqual({ok, <<"https://some/callback1">>}, Actual1),
    Actual2 = auth_oidc_backend:get_redirection_uri(<<"some-id-2">>, app_ctx),
    ?assertEqual({ok, <<"https://some/callback2">>}, Actual2).

associate_access_code_expires_its_associated_access_codes_after_configured_expiry_time() ->
    {ok, app_ctx} = auth_oidc_backend:associate_access_code(<<"code">>, grant_ctx, app_ctx),
    timer:sleep(10),
    Actual = auth_oidc_backend:resolve_access_code(<<"code">>, app_ctx),
    ?assertEqual({error, notfound}, Actual).

-module(auth_hand_oidc_auth_tests).

-include_lib("hoax/include/hoax.hrl").

-compile(export_all).

hoax_fixture_test_() ->
    hoax:fixture(?MODULE).

handle_get_with_code_response_type_when_code_request_is_authorized_returns_saml_redirect() ->
    ClientId = <<"my-client">>,
    ClientStateVal = <<"returnToFancyPage">>,
    RedirectUri = <<"https://mine/callback">>,
    ScopeParam = <<"openid email">>,
    Scopes = [<<"openid">>, <<"email">>],
    ClientState = [{<<"response_type">>, <<"code">>},
                   {<<"state">>, ClientStateVal},
                   {<<"redirect_uri">>, RedirectUri},
                   {<<"client_id">>, ClientId},
                   {<<"scope">>, ScopeParam}],
    User = undefined,
    RelayState = [{oidc_redirect, RedirectUri},
                  {oidc_client_id, ClientId},
                  {oidc_scopes, Scopes},
                  {client_state, ClientStateVal}],

    hoax:mock(cowboy_req,
              ?expect(method,
                      ?withArgs([req]),
                      ?andReturn({<<"GET">>, req2}))),
    hoax:mock(auth_oidc_utils,
              ?expect(auth_request_vals,
                      ?withArgs([req2]),
                      ?andReturn({ok, req3, ClientState}))),
    hoax:mock(oauth2,
              ?expect(authorize_code_request,
                      ?withArgs([User, ClientId, RedirectUri, Scopes, state]),
                      ?andReturn({ok, {app_ctx, auth_ctx}}))),
    hoax:mock(auth_saml_utils, [
              ?expect(encode_relaystate,
                      ?withArgs([RelayState]),
                      ?andReturn(encoded_relaystate)),
              ?expect(make_redirect_target,
                      ?withArgs([encoded_relaystate]),
                      ?andReturn(saml_redirect_location))]),
    hoax:mock(deliv_web_utils,
              ?expect(redirect_302,
                      ?withArgs([saml_redirect_location, req3, state]),
                      ?andReturn({ok, req4, state}))),

    Actual = auth_hand_oidc_auth:handle(req, state),
    ?assertEqual({ok, req4, state}, Actual),
    ?verifyAll.

handle_get_with_code_response_type_when_code_request_is_not_authorized_returns_error_response() ->
    ClientId = <<"my-client">>,
    ClientStateVal = <<"returnToFancyPage">>,
    RedirectUri = <<"https://mine/callback">>,
    ScopeParam = <<"openid email">>,
    Scopes = [<<"openid">>, <<"email">>],
    ClientState = [{<<"response_type">>, <<"code">>},
                   {<<"state">>, ClientStateVal},
                   {<<"redirect_uri">>, RedirectUri},
                   {<<"client_id">>, ClientId},
                   {<<"scope">>, ScopeParam}],
    User = undefined,

    hoax:mock(cowboy_req,
              ?expect(method,
                      ?withArgs([req]),
                      ?andReturn({<<"GET">>, req2}))),
    hoax:mock(auth_oidc_utils,
              ?expect(auth_request_vals,
                      ?withArgs([req2]),
                      ?andReturn({ok, req3, ClientState}))),
    hoax:mock(oauth2,
              ?expect(authorize_code_request,
                      ?withArgs([User, ClientId, RedirectUri, Scopes, state]),
                      ?andReturn({error, reason}))),
    hoax:mock(auth_oidc_utils,
              ?expect(redirect_error_response,
                      ?withArgs([RedirectUri, reason, ClientStateVal, req3, state]),
                      ?andReturn({ok, req4, state}))),

    Actual = auth_hand_oidc_auth:handle(req, state),
    ?assertEqual({ok, req4, state}, Actual),
    ?verifyAll.

handle_get_with_response_type_not_code_returns_error_response() ->
    ClientId = <<"my-client">>,
    ClientStateVal = <<"returnToFancyPage">>,
    RedirectUri = <<"https://mine/callback">>,
    ScopeParam = <<"openid email">>,
    ClientState = [{<<"response_type">>, <<"not_code">>},
                   {<<"state">>, ClientStateVal},
                   {<<"redirect_uri">>, RedirectUri},
                   {<<"client_id">>, ClientId},
                   {<<"scope">>, ScopeParam}],

    hoax:mock(cowboy_req,
              ?expect(method,
                      ?withArgs([req]),
                      ?andReturn({<<"GET">>, req2}))),
    hoax:mock(auth_oidc_utils, [
              ?expect(auth_request_vals,
                      ?withArgs([req2]),
                      ?andReturn({ok, req3, ClientState})),
              ?expect(redirect_error_response,
                      ?withArgs([RedirectUri, unsupported_response_type, ClientStateVal, req3, state]),
                      ?andReturn({halt, req4, state}))]),

    Actual = auth_hand_oidc_auth:handle(req, state),
    ?assertEqual({halt, req4, state}, Actual),
    ?verifyAll.

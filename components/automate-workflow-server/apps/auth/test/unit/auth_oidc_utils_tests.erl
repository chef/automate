-module(auth_oidc_utils_tests).

-include_lib("delivery/include/deliv_types.hrl").
-include_lib("hoax/include/hoax.hrl").
-include_lib("jose/include/jose_jwk.hrl").
-include_lib("jose/include/jose_jwt.hrl").

-compile(export_all).

hoax_fixture_test_() ->
    [hoax:fixture(?MODULE, "access_token_response_"),
     hoax:fixture(?MODULE, "generate_jwt_"),
     hoax:fixture(?MODULE, "issuer_"),
     hoax:fixture(?MODULE, "private_signing_key_"),
     hoax:fixture(?MODULE, "redirect_code_response_"),
     hoax:fixture(?MODULE, "redirect_error_response_"),
     hoax:fixture(?MODULE, "redirect_code_request_"),
     hoax:fixture(?MODULE, "verify_client_redirect_uri_")
    ].

access_token_response_when_given_token_adds_expiry_time_and_type() ->
    Token = <<"sometoken">>,
    Expiry = 3600,
    Type = <<"Bearer">>,

    hoax:mock(oauth2_config,
              ?expect(expiry_time,
                      ?withArgs([]),
                      ?andReturn(Expiry))),

    {ok, Json} = auth_oidc_utils:access_token_response({ok, Token}),
    ?assertEqual(Token, ej:get([<<"id_token">>], Json)),
    ?assertEqual(Expiry, ej:get([<<"expires_in">>], Json)),
    ?assertEqual(Type, ej:get([<<"token_type">>], Json)),
    ?verifyAll.

issuer_returns_the_issuer_url() ->
    ApiBaseUrl = <<"https://foo.bar/api/v0">>,
    hoax:mock(deliv_web_utils,
              ?expect(api_base,
                      ?withArgs([]),
                      ?andReturn(ApiBaseUrl))),

    Actual = auth_oidc_utils:issuer(),
    ?assertEqual(ApiBaseUrl, Actual),
    ?verifyAll.

generate_jwt_when_key_is_defined_and_read_successfully_returns_signed_and_compacted_jwt() ->
    KeyPath = <<"/etc/delivery/signing_key.pem">>,
    EntName = <<"ent">>,
    UserName = <<"sarah">>,
    First = <<"Sarah">>,
    Last = <<"Manning">>,
    Email = <<"sarah@aol.com">>,
    User = deliv_user:fromlist([{name, UserName},
                                {first_name, First},
                                {last_name, Last},
                                {email, Email}]),
    Issuer = <<"https://foo.bar/api/v0">>,
    ClientId = <<"some-client">>,
    ExpiresAt = 1,
    IssuedAt = 0,
    Claims = #jose_jwt{fields = #{
                         sub => UserName,
                         enterprise => EntName,
                         email => Email,
                         first => First,
                         last => Last,
                         exp => ExpiresAt,
                         iat => IssuedAt,
                         iss => Issuer,
                         aud => ClientId}},
    JWS = #{
      <<"alg">> => <<"RS256">>
     },

    hoax:mock(application,
              ?expect(get_env,
                      ?withArgs([auth, oidc_signing_key_file, undefined]),
                      ?andReturn(KeyPath))),
    hoax:mock(jose_jwk,
              ?expect(from_pem_file,
                      ?withArgs([KeyPath]),
                      ?andReturn(#jose_jwk{}))),
    hoax:mock(deliv_web_utils,
              ?expect(api_base,
                      ?withArgs([]),
                      ?andReturn(Issuer))),
    hoax:mock(jose_jwt,
              ?expect(sign,
                      ?withArgs([#jose_jwk{}, JWS, Claims]),
                      ?andReturn(signed_token))),
    hoax:mock(jose_jws,
              ?expect(compact,
                      ?withArgs([signed_token]),
                      ?andReturn({something, compacted_signed_token}))),

    {ok, Actual} = auth_oidc_utils:generate_jwt(EntName, User, ClientId, ExpiresAt, IssuedAt),
    ?assertEqual(compacted_signed_token, Actual),
    ?verifyAll.

generate_jwt_when_key_is_undefined_returns_no_key_error() ->
    EntName = <<"ent">>,
    User = deliv_user:fromlist([{name, <<"Sarah">>}]),
    ClientId = <<"some-client">>,
    ExpiresAt = 1,
    IssuedAt = 0,

    hoax:mock(application,
              ?expect(get_env,
                      ?withArgs([auth, oidc_signing_key_file, undefined]),
                      ?andReturn(undefined))),

    Actual = auth_oidc_utils:generate_jwt(EntName, User, ClientId, ExpiresAt, IssuedAt),
    ?assertEqual({error, no_key}, Actual),
    ?verifyAll.

generate_jwt_when_key_is_defined_but_cannot_be_read_returns_no_key_error() ->
    KeyPath = <<"/etc/delivery/signing_key.pem">>,
    EntName = <<"ent">>,
    User = deliv_user:fromlist([{name, <<"Sarah">>}]),
    ClientId = <<"some-client">>,
    ExpiresAt = 1,
    IssuedAt = 0,

    hoax:mock(application,
              ?expect(get_env,
                      ?withArgs([auth, oidc_signing_key_file, undefined]),
                      ?andReturn(KeyPath))),
    hoax:mock(jose_jwk,
              ?expect(from_pem_file,
                      ?withArgs([KeyPath]),
                      ?andReturn(error))),

    Actual = auth_oidc_utils:generate_jwt(EntName, User, ClientId, ExpiresAt, IssuedAt),
    ?assertEqual({error, no_key}, Actual),
    ?verifyAll.

redirect_code_response_with_code_added_and_clientstate_empty_redirects_to_given_redirecturi() ->
    RedirectUri = <<"https://oidc-client/callback">>,
    Code = <<"23HUSSDHOAS">>,
    ClientState = <<>>,
    ComposedRedirect = <<"https://oidc-client/callback?code=23HUSSDHOAS&state=">>,

    hoax:mock(deliv_web_utils, [
              ?expect(encode_url,
                      ?withArgs([ClientState]),
                      ?andReturn(ClientState)),
              ?expect(redirect_302,
                      ?withArgs([ComposedRedirect, req, state]),
                      ?andReturn({ok, req2, state2}))]),

    Actual = auth_oidc_utils:redirect_code_response(RedirectUri, Code, ClientState, req, state),
    ?assertEqual({ok, req2, state2}, Actual),
    ?verifyAll.

redirect_code_response_when_state_is_nonempty_redirects_to_given_redirecturi_with_code_and_state_params_added() ->
    RedirectUri = <<"https://oidc-client/callback">>,
    Code = <<"23HUSSDHOAS">>,
    ClientState = <<"dazed & confused">>,
    ComposedRedirect = <<"https://oidc-client/callback?code=23HUSSDHOAS&state=dazed%20%26%20confused">>,

    hoax:mock(deliv_web_utils, [
              ?expect(encode_url,
                      ?withArgs([ClientState]),
                      ?andReturn(<<"dazed%20%26%20confused">>)),
              ?expect(redirect_302,
                      ?withArgs([ComposedRedirect, req, state]),
                      ?andReturn({ok, req2, state2}))]),

    Actual = auth_oidc_utils:redirect_code_response(RedirectUri, Code, ClientState, req, state),
    ?assertEqual({ok, req2, state2}, Actual),
    ?verifyAll.

redirect_error_response_redirects_to_given_redirecturi_with_params_added() ->
    RedirectUri = <<"https://oidc-client/callback">>,
    Error = <<"unauthorized_client">>,
    ClientState = <<"dazed & confused">>,
    ComposedRedirect = <<"https://oidc-client/callback?error=unauthorized_client&state=dazed%20%26%20confused">>,

    hoax:mock(deliv_web_utils, [
              ?expect(encode_url,
                      ?withArgs([ClientState]),
                      ?andReturn(<<"dazed%20%26%20confused">>)),
              ?expect(redirect_302,
                      ?withArgs([ComposedRedirect, req, state]),
                      ?andReturn({ok, req2, state2}))]),

    Actual = auth_oidc_utils:redirect_error_response(RedirectUri, Error, ClientState, req, state),
    ?assertEqual({ok, req2, state2}, Actual),
    ?verifyAll.

redirect_error_response_when_given_atom_reason_also_redirects_to_given_redirecturi_with_params_added() ->
    RedirectUri = <<"https://oidc-client/callback">>,
    Error = unauthorized_client,
    ClientState = <<"dazed & confused">>,
    ComposedRedirect = <<"https://oidc-client/callback?error=unauthorized_client&state=dazed%20%26%20confused">>,

    hoax:mock(chef_utils,
              ?expect(to_bin,
                      ?withArgs([Error]),
                      ?andReturn(<<"unauthorized_client">>))),
    hoax:mock(deliv_web_utils, [
              ?expect(encode_url,
                      ?withArgs([ClientState]),
                      ?andReturn(<<"dazed%20%26%20confused">>)),
              ?expect(redirect_302,
                      ?withArgs([ComposedRedirect, req, state]),
                      ?andReturn({ok, req2, state2}))]),

    Actual = auth_oidc_utils:redirect_error_response(RedirectUri, Error, ClientState, req, state),
    ?assertEqual({ok, req2, state2}, Actual),
    ?verifyAll.

redirect_error_response_when_passed_a_client_id_verifies_the_redirect_uri_and_on_error_responds_directly() ->
    ClientId = <<"manage">>,
    BackendUri = <<"https://manage/oidc/callback">>,
    IncomingUri = <<"https://evilfoo/oidc/callback">>,
    hoax:mock(auth_oidc_backend,
              ?expect(get_redirection_uri,
                      ?withArgs([ClientId, app_ctx]),
                      ?andReturn({ok, BackendUri}))),
    hoax:mock(deliv_web_utils,
              ?expect(error_response,
                      ?withArgs([403, forbidden, <<"Redirect URI mismatch">>, req, state]),
                      ?andReturn({halt, req2, state2}))),

    Actual = auth_oidc_utils:redirect_error_response(ClientId, IncomingUri, some_error, client_state, req, state),
    ?assertEqual({ok, req2, state2}, Actual),
    ?verifyAll.

redirect_code_request_when_request_is_authorized_issues_code_and_returns_redirect_with_code() ->
    ClientId = <<"my-client">>,
    ClientStateVal = <<"returnToFancyPage">>,
    RedirectUri = <<"https://mine/callback">>,
    Scopes = [<<"openid">>, <<"email">>],
    EntName = <<"ent">>,
    User = user_record,
    Code = <<"xCSOfXgT6rTj8VbhaFZHd0ORWJX4M12T">>,
    ClientState = [{client_state, ClientStateVal},
                   {oidc_client_id, ClientId},
                   {oidc_redirect, RedirectUri},
                   {oidc_scopes, Scopes}],

    hoax:mock(oauth2, [
              ?expect(authorize_code_request,
                      ?withArgs([{EntName, User}, ClientId, RedirectUri, Scopes, state]),
                      ?andReturn({ok, {app_ctx, auth_ctx}})),
              ?expect(issue_code,
                      ?withArgs([auth_ctx, app_ctx]),
                      ?andReturn({ok, {app_ctx, access_code_response}}))]),
    hoax:mock(oauth2_response,
              ?expect(access_code,
                      ?withArgs([access_code_response]),
                      ?andReturn({ok, Code}))),
    hoax:mock(deliv_web_utils, [
              ?expect(encode_url,
                      ?withArgs([ClientStateVal]),
                      ?andReturn(ClientStateVal)),
              ?expect(redirect_302,
                      ?withArgs([<<"https://mine/callback?code=", Code/binary, "&state=", ClientStateVal/binary>>, req, state]),
                      ?andReturn({ok, req2, state2}))]),

    Actual = auth_oidc_utils:redirect_code_request(ClientState, EntName, User, req, state),
    ?assertEqual({ok, req2, state2}, Actual),
    ?verifyAll.

redirect_code_request_when_request_is_not_authorized_but_redirect_uri_is_valid_returns_redirect_with_error() ->
    ClientId = <<"my-client">>,
    ClientStateVal = <<"returnToFancyPage">>,
    RedirectUri = <<"https://mine/callback">>,
    Scopes = [<<"openid">>, <<"email">>],
    EntName = <<"ent">>,
    User = user_record,
    ClientState = [{client_state, ClientStateVal},
                   {oidc_client_id, ClientId},
                   {oidc_redirect, RedirectUri},
                   {oidc_scopes, Scopes}],

    hoax:mock(oauth2,
              ?expect(authorize_code_request,
                      ?withArgs([{EntName, User}, ClientId, RedirectUri, Scopes, state]),
                      ?andReturn({error, reason}))),
    hoax:mock(auth_oidc_backend,
              ?expect(get_redirection_uri,
                      ?withArgs([ClientId, app_ctx]),
                      ?andReturn({ok, RedirectUri}))),
    hoax:mock(deliv_web_utils, [
              ?expect(encode_url,
                      ?withArgs([ClientStateVal]),
                      ?andReturn(ClientStateVal)),
              ?expect(redirect_302,
                      ?withArgs([<<"https://mine/callback?error=reason&state=", ClientStateVal/binary>>, req, state]),
                      ?andReturn({ok, req2, state2}))]),

    Actual = auth_oidc_utils:redirect_code_request(ClientState, EntName, User, req, state),
    ?assertEqual({ok, req2, state2}, Actual),
    ?verifyAll.

redirect_code_request_when_request_is_not_authorized_and_redirect_uri_is_invalid_returns_direct_error_response() ->
    ClientId = <<"my-client">>,
    ClientStateVal = <<"returnToFancyPage">>,
    RedirectUri = <<"https://hostile/callback">>,
    Scopes = [<<"openid">>, <<"email">>],
    EntName = <<"ent">>,
    User = user_record,
    ClientState = [{client_state, ClientStateVal},
                   {oidc_client_id, ClientId},
                   {oidc_redirect, RedirectUri},
                   {oidc_scopes, Scopes}],

    hoax:mock(oauth2,
              ?expect(authorize_code_request,
                      ?withArgs([{EntName, User}, ClientId, RedirectUri, Scopes, state]),
                      ?andReturn({error, reason}))),
    hoax:mock(auth_oidc_backend,
              ?expect(get_redirection_uri,
                      ?withArgs([ClientId, app_ctx]),
                      ?andReturn({error, reason}))),
    hoax:mock(deliv_web_utils,
              ?expect(error_response,
                      ?withArgs([403, forbidden, <<"Redirect URI mismatch">>, req, state]),
                      ?andReturn({halt, req2, state2}))),

    Actual = auth_oidc_utils:redirect_code_request(ClientState, EntName, User, req, state),
    ?assertEqual({ok, req2, state2}, Actual),
    ?verifyAll.

private_signing_key_when_key_is_configured_returns_key() ->
    KeyPath = <<"/etc/delivery/signing_key.pem">>,
    hoax:mock(application,
              ?expect(get_env,
                      ?withArgs([auth, oidc_signing_key_file, undefined]),
                      ?andReturn(KeyPath))),
    hoax:mock(jose_jwk,
              ?expect(from_pem_file,
                      ?withArgs([KeyPath]),
                      ?andReturn(#jose_jwk{}))),

    Actual = auth_oidc_utils:private_signing_key(),
    ?assertEqual({ok, #jose_jwk{}}, Actual),
    ?verifyAll.

private_signing_key_when_key_is_configured_but_cannot_be_read_returns_error() ->
    KeyPath = <<"/etc/delivery/signing_key.pem">>,
    hoax:mock(application,
              ?expect(get_env,
                      ?withArgs([auth, oidc_signing_key_file, undefined]),
                      ?andReturn(KeyPath))),
    hoax:mock(jose_jwk,
              ?expect(from_pem_file,
                      ?withArgs([KeyPath]),
                      ?andReturn([]))),

    Actual = auth_oidc_utils:private_signing_key(),
    ?assertEqual({error, no_key}, Actual),
    ?verifyAll.

private_signing_key_when_key_is_not_configured_returns_error() ->
    hoax:mock(application,
              ?expect(get_env,
                      ?withArgs([auth, oidc_signing_key_file, undefined]),
                      ?andReturn(undefined))),

    Actual = auth_oidc_utils:private_signing_key(),
    ?assertEqual({error, no_key}, Actual),
    ?verifyAll.

verify_client_redirect_uri_when_client_id_is_known_and_backend_and_argument_uri_match_returns_ok() ->
    ClientId = <<"manage">>,
    Uri = <<"https://manage/oidc/callback">>,
    hoax:mock(auth_oidc_backend,
              ?expect(get_redirection_uri,
                      ?withArgs([ClientId, app_ctx]),
                      ?andReturn({ok, Uri}))),
    Actual = auth_oidc_utils:verify_client_redirect_uri(ClientId, Uri),
    ?assertEqual(ok, Actual),
    ?verifyAll.

verify_client_redirect_uri_when_client_id_is_known_and_backend_and_argument_uri_do_not_match_returns_error() ->
    ClientId = <<"manage">>,
    BackendUri = <<"https://manage/oidc/callback">>,
    IncomingUri = <<"https://evilfoo/oidc/callback">>,
    hoax:mock(auth_oidc_backend,
              ?expect(get_redirection_uri,
                      ?withArgs([ClientId, app_ctx]),
                      ?andReturn({ok, BackendUri}))),
    Actual = auth_oidc_utils:verify_client_redirect_uri(ClientId, IncomingUri),
    ?assertEqual(error, Actual),
    ?verifyAll.

verify_client_redirect_uri_when_client_id_is_unknown_returns_error() ->
    ClientId = <<"manage">>,
    IncomingUri = <<"https://evilfoo/oidc/callback">>,
    hoax:mock(auth_oidc_backend,
              ?expect(get_redirection_uri,
                      ?withArgs([ClientId, app_ctx]),
                      ?andReturn({error, notfound}))),
    Actual = auth_oidc_utils:verify_client_redirect_uri(ClientId, IncomingUri),
    ?assertEqual(error, Actual),
    ?verifyAll.

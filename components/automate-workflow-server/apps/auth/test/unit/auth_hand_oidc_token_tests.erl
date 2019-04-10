-module(auth_hand_oidc_token_tests).

-include_lib("hoax/include/hoax.hrl").

-compile(export_all).

hoax_fixture_test_() ->
    hoax:fixture(?MODULE).

handle_post_with_authorization_code_grant_type_when_code_grant_is_authorized_returns_access_token_response() ->
    ClientId = <<"my-client">>,
    ClientSecret = <<"my-secret">>,
    ClientStateVal = <<"returnToFancyPage">>,
    RedirectUri = <<"https://mine/callback">>,
    Code = <<"xCSOfXgT6rTj8VbhaFZHd0ORWJX4M12T">>,
    FormData = [{<<"grant_type">>, <<"authorization_code">>},
                {<<"state">>, ClientStateVal},
                {<<"redirect_uri">>, RedirectUri},
                {<<"client_id">>, ClientId},
                {<<"client_secret">>, ClientSecret},
                {<<"code">>, Code}],
    EntName = <<"Ent">>,
    User = user_record,

    hoax:mock(cowboy_req, [
              ?expect(method,
                      ?withArgs([req]),
                      ?andReturn({<<"POST">>, req2})),
              ?expect(parse_header,
                      ?withArgs([<<"content-type">>, req2]),
                      ?andReturn({ok, {<<"application">>, <<"x-www-form-urlencoded">>, []}, req3})),
              ?expect(body_qs,
                      ?withArgs([req3]),
                      ?andReturn({ok, FormData, req4}))]),
    hoax:mock(oauth2,
              ?expect(authorize_code_grant,
                      ?withArgs([{ClientId, ClientSecret}, Code, RedirectUri, none]),
                      ?andReturn({ok, {app_ctx, auth_ctx}}))),
    hoax:mock(auth_oidc_backend, [
              ?expect(get_enterprise,
                      ?withArgs([auth_ctx]),
                      ?andReturn(EntName)),
              ?expect(get_user,
                      ?withArgs([auth_ctx]),
                      ?andReturn(User))]),
    hoax:mock(auth_oidc_utils,
              ?expect(access_token_response,
                      ?withArgs([EntName, User, ClientId]),
                      ?andReturn({ok, ejson}))),
    hoax:mock(deliv_web_utils,
              ?expect(content,
                      ?withArgs([ejson, req4, state]),
                      ?andReturn({body, req5, state2}))),
    hoax:mock(cowboy_req,
              ?expect(reply,
                      ?withArgs([200, [], body, req5]),
                      ?andReturn({ok, req6}))),

    Actual = auth_hand_oidc_token:handle(req, state),
    ?assertEqual({ok, req6, state2}, Actual),
    ?verifyAll.

handle_post_with_authorization_code_grant_type_when_code_grant_is_not_authorized_returns_invalid_grant_error_response() ->
    ClientId = <<"my-client">>,
    ClientSecret = <<"my-secret">>,
    RedirectUri = <<"https://mine/callback">>,
    Code = <<"xCSOfXgT6rTj8VbhaFZHd0ORWJX4M12T">>,
    FormData = [{<<"grant_type">>, <<"authorization_code">>},
                {<<"redirect_uri">>, RedirectUri},
                {<<"client_id">>, ClientId},
                {<<"client_secret">>, ClientSecret},
                {<<"code">>, Code}],

    hoax:mock(cowboy_req, [
              ?expect(method,
                      ?withArgs([req]),
                      ?andReturn({<<"POST">>, req2})),
              ?expect(parse_header,
                      ?withArgs([<<"content-type">>, req2]),
                      ?andReturn({ok, {<<"application">>, <<"x-www-form-urlencoded">>, []}, req3})),
              ?expect(body_qs,
                      ?withArgs([req3]),
                      ?andReturn({ok, FormData, req4}))]),
    hoax:mock(oauth2,
              ?expect(authorize_code_grant,
                      ?withArgs([{ClientId, ClientSecret}, Code, RedirectUri, none]),
                      ?andReturn({error, invalid_grant}))),
    hoax:mock(deliv_web_utils,
              ?expect(error_response,
                      ?withArgs([400, invalid_grant, req4, state]),
                      ?andReturn({halt, req5, state2}))),

    Actual = auth_hand_oidc_token:handle(req, state),
    ?assertEqual({ok, req5, state2}, Actual),
    ?verifyAll.

handle_post_with_grant_type_not_authorization_code_returns_unsupported_grant_type_error_response() ->
    FormData = [{<<"grant_type">>, <<"something else">>}],

    hoax:mock(cowboy_req, [
              ?expect(method,
                      ?withArgs([req]),
                      ?andReturn({<<"POST">>, req2})),
              ?expect(parse_header,
                      ?withArgs([<<"content-type">>, req2]),
                      ?andReturn({ok, {<<"application">>, <<"x-www-form-urlencoded">>, []}, req3})),
              ?expect(body_qs,
                      ?withArgs([req3]),
                      ?andReturn({ok, FormData, req4}))]),
    hoax:mock(deliv_web_utils, [
              ?expect(error_response,
                      ?withArgs([400, unsupported_grant_type, req4, state]),
                      ?andReturn({halt, req5, state2}))]),

    Actual = auth_hand_oidc_token:handle(req, state),
    ?assertEqual({ok, req5, state2}, Actual),
    ?verifyAll.

handle_post_with_authorization_code_grant_type_when_no_key_is_configured_returns_internal_server_error() ->
    ClientId = <<"my-client">>,
    ClientSecret = <<"my-secret">>,
    RedirectUri = <<"https://mine/callback">>,
    Code = <<"xCSOfXgT6rTj8VbhaFZHd0ORWJX4M12T">>,
    FormData = [{<<"grant_type">>, <<"authorization_code">>},
                {<<"redirect_uri">>, RedirectUri},
                {<<"client_id">>, ClientId},
                {<<"client_secret">>, ClientSecret},
                {<<"code">>, Code}],
    EntName = <<"Ent">>,
    User = user_record,

    hoax:mock(cowboy_req, [
              ?expect(method,
                      ?withArgs([req]),
                      ?andReturn({<<"POST">>, req2})),
              ?expect(parse_header,
                      ?withArgs([<<"content-type">>, req2]),
                      ?andReturn({ok, {<<"application">>, <<"x-www-form-urlencoded">>, []}, req3})),
              ?expect(body_qs,
                      ?withArgs([req3]),
                      ?andReturn({ok, FormData, req4}))]),
    hoax:mock(oauth2,
              ?expect(authorize_code_grant,
                      ?withArgs([{ClientId, ClientSecret}, Code, RedirectUri, none]),
                      ?andReturn({ok, {app_ctx, auth_ctx}}))),
    hoax:mock(auth_oidc_backend, [
              ?expect(get_enterprise,
                      ?withArgs([auth_ctx]),
                      ?andReturn(EntName)),
              ?expect(get_user,
                      ?withArgs([auth_ctx]),
                      ?andReturn(User))]),
    hoax:mock(auth_oidc_utils,
              ?expect(access_token_response,
                      ?withArgs([EntName, User, ClientId]),
                      ?andReturn({error, no_key}))),
    hoax:mock(deliv_web_utils,
              ?expect(error_response,
                      ?withArgs([500, internal_server_error, req4, state]),
                      ?andReturn({halt, req5, state2}))),

    Actual = auth_hand_oidc_token:handle(req, state),
    ?assertEqual({ok, req5, state2}, Actual),
    ?verifyAll.

handle_post_when_content_type_is_not_x_form_www_urlencoded_returns_invalid_request_error() ->
    hoax:mock(cowboy_req, [
              ?expect(method,
                      ?withArgs([req]),
                      ?andReturn({<<"POST">>, req2})),
              ?expect(parse_header,
                      ?withArgs([<<"content-type">>, req2]),
                      ?andReturn({ok, {<<"application">>, <<"json">>, []}, req3}))]),
    hoax:mock(deliv_web_utils,
              ?expect(error_response,
                      ?withArgs([400, invalid_request, req3, state]),
                      ?andReturn({halt, req4, state2}))),

    Actual = auth_hand_oidc_token:handle(req, state),
    ?assertEqual({ok, req4, state2}, Actual),
    ?verifyAll.

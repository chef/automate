-module(auth_hand_saml_auth_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_types.hrl").
-include_lib("esaml/include/esaml.hrl").

-compile(export_all).

hoax_fixture_test_() ->
    hoax:fixture(?MODULE).

handle_when_saml_config_is_present_and_set_to_redirect_binding_redirects_client() ->
    EntName = <<"testEnt">>,
    hoax:mock(deliv_web_utils,
              ?expect(extract_bindings,
                      ?withArgs([[ent_name], req]),
                      ?andReturn({[EntName], req1}))),
    hoax:mock(cowboy_req,
              ?expect(qs_val,
                      ?withArgs([<<"returnUrl">>, req1]),
                      ?andReturn({undefined, req2}))),
    hoax:mock(deliv_web_utils,
              ?expect(redirect_302,
                      ?withArgs([redirect_uri, req2, state]),
                      ?andReturn({ok, req3, state}))),
    hoax:mock(auth_saml_utils, [
              ?expect(make_redirect_target,
                      ?withArgs([EntName, <<>>]),
                      ?andReturn(redirect_uri)),
              ?expect(relay_url,
                      ?withArgs([undefined]),
                      ?andReturn(<<>>))]),

    Actual = auth_hand_saml_auth:handle(req, state),
    ?assertEqual({ok, req3, state}, Actual),
    ?verifyAll.

handle_when_saml_config_is_present_and_set_to_post_binding_redirects_with_not_implemented_message() ->
    EntName = <<"testEnt">>,
    hoax:mock(deliv_web_utils,
              ?expect(extract_bindings,
                      ?withArgs([[ent_name], req]),
                      ?andReturn({[EntName], req1}))),
    hoax:mock(cowboy_req,
              ?expect(qs_val,
                      ?withArgs([<<"returnUrl">>, req1]),
                      ?andReturn({undefined, req2}))),
    hoax:mock(auth_saml_utils, [
              ?expect(make_redirect_target,
                      ?withArgs([EntName, <<>>]),
                      ?andReturn(login_error_redirect_uri)),
              ?expect(relay_url,
                      ?withArgs([undefined]),
                      ?andReturn(<<>>))]),
    hoax:mock(deliv_web_utils,
              ?expect(redirect_302,
                      ?withArgs([login_error_redirect_uri, req2, state]),
                      ?andReturn({ok, req3, state}))),


    Acutal = auth_hand_saml_auth:handle(req, state),
    ?assertEqual({ok, req3, state}, Acutal),
    ?verifyAll.

handle_when_saml_config_is_not_found_redirects_with_error_message() ->
    EntName = <<"testEnt">>,
    hoax:mock(deliv_web_utils,
              ?expect(extract_bindings,
                      ?withArgs([[ent_name], req]),
                      ?andReturn({[EntName], req1}))),
    hoax:mock(cowboy_req,
              ?expect(qs_val,
                      ?withArgs([<<"returnUrl">>, req1]),
                      ?andReturn({undefined, req2}))),
    hoax:mock(auth_saml_utils, [
              ?expect(make_redirect_target,
                      ?withArgs([EntName, <<>>]),
                      ?andReturn(login_error_redirect_uri)),
              ?expect(relay_url,
                      ?withArgs([undefined]),
                      ?andReturn(<<>>))]),
    hoax:mock(deliv_web_utils,
              ?expect(redirect_302,
                      ?withArgs([login_error_redirect_uri, req2, state]),
                      ?andReturn({ok, req3, state}))),

    Actual = auth_hand_saml_auth:handle(req, state),
    ?assertEqual({ok, req3, state}, Actual),
    ?verifyAll.

handle_when_saml_config_sp_errors_redirects_with_error_message() ->
    EntName = <<"testEnt">>,
    hoax:mock(deliv_web_utils,
              ?expect(extract_bindings,
                      ?withArgs([[ent_name], req]),
                      ?andReturn({[EntName], req1}))),
    hoax:mock(cowboy_req,
              ?expect(qs_val,
                      ?withArgs([<<"returnUrl">>, req1]),
                      ?andReturn({undefined, req2}))),
    hoax:mock(deliv_web_utils,
              ?expect(redirect_302,
                      ?withArgs([login_error_redirect_uri, req2, state]),
                      ?andReturn({ok, req3, state}))),
    hoax:mock(auth_saml_utils, [
              ?expect(make_redirect_target,
                      ?withArgs([EntName, <<>>]),
                      ?andReturn(login_error_redirect_uri)),
              ?expect(relay_url,
                      ?withArgs([undefined]),
                      ?andReturn(<<>>))]),

    Actual = auth_hand_saml_auth:handle(req, state),
    ?assertEqual({ok, req3, state}, Actual),
    ?verifyAll.

handle_when_saml_config_is_present_and_set_to_redirect_binding_and_returnUrl_querystring_redirects_client_with_proper_replaystate() ->
    EntName = <<"testEnt">>,
    ReturnUrl = <<"%23%252Ftoken">>,
    hoax:mock(deliv_web_utils,
              ?expect(extract_bindings,
                      ?withArgs([[ent_name], req]),
                      ?andReturn({[EntName], req1}))),
    hoax:mock(cowboy_req,
              ?expect(qs_val,
                      ?withArgs([<<"returnUrl">>, req1]),
                      ?andReturn({ReturnUrl, req2}))),
    hoax:mock(deliv_web_utils,
              ?expect(redirect_302,
                      ?withArgs([redirect_uri, req2, state]),
                      ?andReturn({ok, req3, state}))),
    hoax:mock(auth_saml_utils, [
              ?expect(relay_url,
                      ?withArgs([ReturnUrl]),
                      ?andReturn(encoded_relaystate)),
              ?expect(make_redirect_target,
                      ?withArgs([EntName, encoded_relaystate]),
                      ?andReturn(redirect_uri))]),

    Actual = auth_hand_saml_auth:handle(req, state),
    ?assertEqual({ok, req3, state}, Actual),
    ?verifyAll.

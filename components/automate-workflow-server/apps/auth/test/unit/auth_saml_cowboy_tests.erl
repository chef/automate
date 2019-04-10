-module(auth_saml_cowboy_tests).

-include_lib("delivery/include/deliv_types.hrl").
-include_lib("hoax/include/hoax.hrl").

-compile(export_all).

hoax_fixture_test_() ->
    hoax:fixture(?MODULE, "extract_saml_params_").

extract_saml_params_when_relaystate_decoding_succeeds_returns_decoded_relaystate() ->
    PostVals = [{<<"SAMLEncoding">>, saml_encoding},
                {<<"SAMLResponse">>, saml_response},
                {<<"RelayState">>, encoded_relaystate}],
    hoax:mock(cowboy_req,
              ?expect(body_qs,
                      ?withArgs([req]),
                      ?andReturn({ok, PostVals, req2}))),
    hoax:mock(auth_saml_utils,
              ?expect(decode_relaystate,
                      ?withArgs([encoded_relaystate]),
                      ?andReturn({ok, relaystate}))),

    Actual = auth_saml_cowboy:extract_saml_params(req),

    ?assertEqual({saml_encoding, saml_response, relaystate, req2}, Actual),
    ?verifyAll.

extract_saml_params_when_relaystate_decoding_fails_returns_empty_relaystate() ->
    PostVals = [{<<"SAMLEncoding">>, saml_encoding},
                {<<"SAMLResponse">>, saml_response},
                {<<"RelayState">>, encoded_relaystate}],
    hoax:mock(cowboy_req,
              ?expect(body_qs,
                      ?withArgs([req]),
                      ?andReturn({ok, PostVals, req2}))),
    hoax:mock(auth_saml_utils,
              ?expect(decode_relaystate,
                      ?withArgs([encoded_relaystate]),
                      ?andReturn({error, reason}))),

    Actual = auth_saml_cowboy:extract_saml_params(req),

    ?assertEqual({saml_encoding, saml_response, [], req2}, Actual),
    ?verifyAll.

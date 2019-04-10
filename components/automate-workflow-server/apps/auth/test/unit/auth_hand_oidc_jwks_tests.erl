-module(auth_hand_oidc_jwks_tests).

-include_lib("hoax/include/hoax.hrl").

-compile(export_all).

hoax_fixture_test_() ->
    hoax:fixture(?MODULE).

to_json_when_key_is_configured_and_read_successfully_returns_jwk_set_json() ->
    KeyMap = #{
      <<"kty">> => <<"RSA">>,
      <<"e">> => <<"AQAB">>,
      <<"n">> => <<"4GEogBP...bXQ">>
     },
    Json = {[{<<"keys">>, [
                           {[
                             {<<"alg">>, <<"RS256">>},
                             {<<"e">>, <<"AQAB">>},
                             {<<"kid">>, <<"1">>},
                             {<<"kty">>, <<"RSA">>},
                             {<<"n">>, <<"4GEogBP...bXQ">>},
                             {<<"use">>, <<"sig">>}
                            ]}
                          ]}
            ]},
    hoax:mock(cowboy_req,
              ?expect(set_resp_header,
                      ?withArgs([<<"Cache-Control">>, <<"public, max-age=86400">>, req]),
                      ?andReturn(req2))),
    hoax:mock(auth_oidc_utils,
              ?expect(private_signing_key,
                      ?withArgs([]),
                      ?andReturn({ok, priv_key}))),
    hoax:mock(jose_jwk,
              ?expect(to_public_map,
                      ?withArgs([priv_key]),
                      ?andReturn({record, KeyMap}))),
    hoax:mock(deliv_web_utils,
              ?expect(content,
                      ?withArgs([Json, req2, state]),
                      ?andReturn({ok, req3, state}))),

    Actual = auth_hand_oidc_jwks:to_json(req, state),
    ?assertEqual({ok, req3, state}, Actual),
    ?verifyAll.

to_json_when_key_is_not_configured_or_not_read_successfully_returns_empty_jwk_set_json() ->
    Json = {[
             {<<"keys">>, []}
            ]},
    hoax:mock(cowboy_req,
              ?expect(set_resp_header,
                      ?withArgs([<<"Cache-Control">>, <<"public, max-age=86400">>, req]),
                      ?andReturn(req2))),
    hoax:mock(auth_oidc_utils,
              ?expect(private_signing_key,
                      ?withArgs([]),
                      ?andReturn({error, no_key}))),
    hoax:mock(deliv_web_utils,
              ?expect(content,
                      ?withArgs([Json, req2, state]),
                      ?andReturn({ok, req3, state}))),

    Actual = auth_hand_oidc_jwks:to_json(req, state),
    ?assertEqual({ok, req3, state}, Actual),
    ?verifyAll.

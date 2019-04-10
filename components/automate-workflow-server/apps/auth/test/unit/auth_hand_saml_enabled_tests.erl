-module(auth_hand_saml_enabled_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_types.hrl").

-compile(export_all).

to_json_fixture_test_() ->
    hoax:fixture(?MODULE, "to_json").

allowed_methods_allows_GET_test() ->
    ?assertEqual({[<<"GET">>], req, #handler{}},
                 auth_hand_saml_enabled:allowed_methods(req, #handler{})).

content_types_provided_provides_json_test() ->
    hoax:test(fun() ->
        hoax:mock(deliv_web_utils,
                  ?expect(content_type_json_map,
                          ?withArgs([to_json]),
                          ?andReturn(expected_map))),

        Actual = auth_hand_saml_enabled:content_types_provided(req, #handler{}),

        ?assertEqual({expected_map, req, #handler{}}, Actual),
        ?verifyAll
    end).

to_json_when_saml_configured_returns_true() ->
    EntName = <<"enterprise">>,

    hoax:mock(deliv_web_utils, [
              ?expect(extract_bindings,
                      ?withArgs([[ent_name], req]),
                      ?andReturn({[EntName], req1})),
              ?expect(content,
                      ?withArgs([{[{<<"enabled">>, true}]}, req1, state]),
                      ?andReturn({body, req2, state}))]),
    hoax:mock(auth_saml_config,
              ?expect(fetch,
                      ?withArgs([EntName]),
                      ?andReturn({ok, config}))),

    Actual = auth_hand_saml_enabled:to_json(req, state),
    ?assertEqual({body, req2, state}, Actual),
    ?verifyAll.

to_json_when_saml_not_configured_returns_false() ->
    EntName = <<"enterprise">>,

    hoax:mock(deliv_web_utils, [
              ?expect(extract_bindings,
                      ?withArgs([[ent_name], req]),
                      ?andReturn({[EntName], req1})),
              ?expect(content,
                      ?withArgs([{[{<<"enabled">>, false}]}, req1, state]),
                      ?andReturn({body, req2, state}))]),
    hoax:mock(auth_saml_config,
              ?expect(fetch,
                      ?withArgs([EntName]),
                      ?andReturn({error, not_found}))),

    Actual = auth_hand_saml_enabled:to_json(req, state),
    ?assertEqual({body, req2, state}, Actual),
    ?verifyAll.

to_json_when_db_fetch_errors_return_500() ->
    EntName = <<"enterprise">>,

    hoax:mock(deliv_web_utils, [
              ?expect(extract_bindings,
                      ?withArgs([[ent_name], req]),
                      ?andReturn({[EntName], req1})),
              ?expect(error_response,
                      ?withArgs([500, internal_server_error, req1, state]),
                      ?andReturn(error))]),
    hoax:mock(auth_saml_config,
              ?expect(fetch,
                      ?withArgs([EntName]),
                      ?andReturn({error, why}))),

    Actual = auth_hand_saml_enabled:to_json(req, state),
    ?assertEqual(error, Actual),
    ?verifyAll.

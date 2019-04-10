-module(auth_hand_saml_config_tests).

-include("auth_types.hrl").
-include_lib("hoax/include/hoax.hrl").

-compile(export_all).

hoax_fixture_test_() ->
    hoax:fixture(?MODULE).

allowed_methods_allows_PUT_GET_DELETE() ->
    ?assertEqual({[<<"GET">>, <<"PUT">>, <<"DELETE">>], req, #handler{}},
                 auth_hand_saml_config:allowed_methods(req, #handler{})).

content_types_accepted_accepts_json() ->
    hoax:mock(deliv_web_utils,
              ?expect(content_type_json_map,
                      ?withArgs([from_json]),
                      ?andReturn(expected_map))),

    Actual = auth_hand_saml_config:content_types_accepted(req, #handler{}),

    ?assertEqual({expected_map, req, #handler{}}, Actual),
    ?verifyAll.

content_types_provided_provides_json() ->
    hoax:mock(deliv_web_utils,
              ?expect(content_type_json_map,
                      ?withArgs([to_json]),
                      ?andReturn(expected_map))),

    Actual = auth_hand_saml_config:content_types_provided(req, #handler{}),

    ?assertEqual({expected_map, req, #handler{}}, Actual),
    ?verifyAll.

delete_resource_deletes_saml_configuration_for_enterprise_returns_204_on_success() ->
    EntName = <<"NCC-1701">>,
    State = #handler{ent_name = EntName},
    hoax:mock(auth_saml_config,
              ?expect(delete,
                      ?withArgs([EntName]),
                      ?andReturn(ok))),

    Actual = auth_hand_saml_config:delete_resource(req, State),
    ?assertEqual({true, req, State}, Actual),
    ?verifyAll.

delete_resource_returns_500_on_failure() ->
    EntName = <<"NCC-1701">>,
    State = #handler{ent_name = EntName},
    hoax:mock(auth_saml_config,
              ?expect(delete,
                      ?withArgs([EntName]),
                      ?andReturn({error, not_found}))),
    hoax:mock(deliv_web_utils,
              ?expect(error_response,
                      ?withArgs([500, internal_server_error, req, State]),
                      ?andReturn({halt, req2, State}))),

    Actual = auth_hand_saml_config:delete_resource(req, State),
    ?assertEqual({halt, req2, State}, Actual),
    ?verifyAll.

from_json_when_upsert_of_metadata_saml_configuration_succeeds_returns_201() ->
    EntName = <<"NCC-1701">>,
    SSOBinding = <<"HTTP-Redirect">>,
    EntName = <<"NCC-1701">>,
    MetadataUrl = <<"https://bouncer/metadata-1701">>,
    NameId = <<"urn:oasis:names:tc:SAML:2.0:nameid-format:entity">>,
    Config = #saml_config{sso_binding = SSOBinding,
                          ent_name = EntName,
                          metadata_url = MetadataUrl,
                          name_id = NameId},
    hoax:mock(deliv_web_utils,
              ?expect(parse_json_req,
                      ?withArgs([req, saml_config]),
                      ?andReturn({req_json_body, req1}))),
    hoax:mock(cowboy_req,
              ?expect(binding,
                      ?withArgs([ent_name, req1]),
                      ?andReturn({EntName, req2}))),
    hoax:mock(auth_saml_config, [
              ?expect(from_json,
                      ?withArgs([req_json_body, EntName]),
                      ?andReturn(Config)),
              ?expect(refresh_metadata,
                      ?withArgs([Config]),
                      ?andReturn({ok, Config})),
              ?expect(upsert,
                      ?withArgs([Config]),
                      ?andReturn({ok, Config}))]),

    Response = auth_hand_saml_config:from_json(req, state),
    ?assertEqual({true, req2, state}, Response),
    ?verifyAll.

from_json_when_upsert_of_saml_configuration_succeeds_returns_201() ->
    SSOLoginUrl = <<"https://bomb.com/login">>,
    SSOBinding = <<"HTTP-Redirect">>,
    IdPUrl = <<"https://bomb.com">>,
    Cert =  <<"x509abcabcabc">>,
    EntName = <<"NCC-1701">>,
    NameId = <<"urn:oasis:names:tc:SAML:2.0:nameid-format:entity">>,
    Config = #saml_config{sso_login_url = SSOLoginUrl,
                          sso_binding = SSOBinding,
                          idp_url = IdPUrl,
                          cert = Cert,
                          ent_name = EntName,
                          metadata_url = undefined,
                          name_id = NameId},

    hoax:mock(deliv_web_utils,
              ?expect(parse_json_req,
                      ?withArgs([req, saml_config]),
                      ?andReturn({req_json_body, req1}))),
    hoax:mock(cowboy_req,
              ?expect(binding,
                      ?withArgs([ent_name, req1]),
                      ?andReturn({EntName, req2}))),
    hoax:mock(auth_saml_config, [
              ?expect(from_json,
                      ?withArgs([req_json_body, EntName]),
                      ?andReturn(Config)),
              ?expect(refresh_metadata,
                      ?withArgs([Config]),
                      ?andReturn({ok, Config})),
              ?expect(upsert,
                      ?withArgs([Config]),
                      ?andReturn({ok, Config}))]),

    Response = auth_hand_saml_config:from_json(req, state),
    ?assertEqual({true, req2, state}, Response),
    ?verifyAll.

from_json_returns_400_bad_request_for_invalid_json() ->
    hoax:mock(deliv_web_utils,[
              ?expect(parse_json_req,
                      ?withArgs([req, saml_config]),
                      ?andReturn({{error, because}, req1})),
              ?expect(error_response,
                      ?withArgs([400, bad_request, req1, state]),
                      ?andReturn({halt, req2, state}))]),

    Response = auth_hand_saml_config:from_json(req, state),
    ?assertEqual({halt, req2, state}, Response),
    ?verifyAll.

from_json_with_missing_default_roles_returns_400_bad_request_with_error_message() ->
    ValidationError = {error, {[{data_invalid,
                                {[{<<"type">>,<<"array">>},
                                  {<<"required">>,true},
                                  {<<"items">>,{[{<<"type">>,<<"string">>}]}},
                                  {<<"minItems">>,1}]},
                                 wrong_size,
                                 [],
                                 [<<"default_roles">>]}],
                                 [{<<"default_roles">>, []}]}},
    ErrorMessage = <<"Please select at least one default role.">>,

    hoax:mock(deliv_web_utils,[
              ?expect(parse_json_req,
                      ?withArgs([req, saml_config]),
                      ?andReturn({ValidationError, req1})),
              ?expect(error_response,
                      ?withArgs([400, bad_request, ErrorMessage, req1, state]),
                      ?andReturn({halt, req2, state}))]),

    Response = auth_hand_saml_config:from_json(req, state),
    ?assertEqual({halt, req2, state}, Response),
    ?verifyAll.

from_json_when_metadata_refresh_yields_unavailable_xml_returns_400_bad_request() ->
    EntName = <<"NCC-1701">>,
    Error = <<"received status 404 when retrieving metadata">>,

    hoax:mock(deliv_web_utils,
              ?expect(parse_json_req,
                      ?withArgs([req, saml_config]),
                      ?andReturn({req_json_body, req1}))),
    hoax:mock(cowboy_req,
              ?expect(binding,
                      ?withArgs([ent_name, req1]),
                      ?andReturn({EntName, req2}))),
    hoax:mock(auth_saml_config, [
              ?expect(from_json,
                      ?withArgs([req_json_body, EntName]),
                      ?andReturn(saml_config)),
              ?expect(refresh_metadata,
                      ?withArgs([saml_config]),
                      ?andReturn({error, status, 404}))]),
    hoax:mock(deliv_web_utils,
              ?expect(error_response,
                      ?withArgs([400, bad_request, Error, req2, state]),
                      ?andReturn({halt, req3, state}))),

    Response = auth_hand_saml_config:from_json(req, state),
    ?assertEqual({halt, req3, state}, Response),
    ?verifyAll.

from_json_when_metadata_refresh_returns_unexpected_status_returns_400_bad_request() ->
    EntName = <<"NCC-1701">>,
    Error = <<"received status 301 when retrieving metadata">>,

    hoax:mock(deliv_web_utils,
              ?expect(parse_json_req,
                      ?withArgs([req, saml_config]),
                      ?andReturn({req_json_body, req1}))),
    hoax:mock(cowboy_req,
              ?expect(binding,
                      ?withArgs([ent_name, req1]),
                      ?andReturn({EntName, req2}))),
    hoax:mock(auth_saml_config, [
              ?expect(from_json,
                      ?withArgs([req_json_body, EntName]),
                      ?andReturn(saml_config)),
              ?expect(refresh_metadata,
                      ?withArgs([saml_config]),
                      ?andReturn({error, status, 301}))]),
    hoax:mock(deliv_web_utils,
              ?expect(error_response,
                      ?withArgs([400, bad_request, Error, req2, state]),
                      ?andReturn({halt, req3, state}))),

    Response = auth_hand_saml_config:from_json(req, state),
    ?assertEqual({halt, req3, state}, Response),
    ?verifyAll.

from_json_when_metadata_refresh_xml_size_exceeded_limit_returns_400_bad_request() ->
    EntName = <<"NCC-1701">>,
    Error = <<"metadata file exceeded limit: 1000000 bytes">>,

    hoax:mock(deliv_web_utils,
              ?expect(parse_json_req,
                      ?withArgs([req, saml_config]),
                      ?andReturn({req_json_body, req1}))),
    hoax:mock(cowboy_req,
              ?expect(binding,
                      ?withArgs([ent_name, req1]),
                      ?andReturn({EntName, req2}))),
    hoax:mock(auth_saml_config, [
              ?expect(from_json,
                      ?withArgs([req_json_body, EntName]),
                      ?andReturn(saml_config)),
              ?expect(refresh_metadata,
                      ?withArgs([saml_config]),
                      ?andReturn({error, too_big}))]),
    hoax:mock(deliv_web_utils,
              ?expect(error_response,
                      ?withArgs([400, bad_request, Error, req2, state]),
                      ?andReturn({halt, req3, state}))),

    Response = auth_hand_saml_config:from_json(req, state),
    ?assertEqual({halt, req3, state}, Response),
    ?verifyAll.

from_json_when_upsert_fails_returns_500() ->
    EntName = <<"NCC-1701">>,
    hoax:mock(deliv_web_utils,
              ?expect(parse_json_req,
                      ?withArgs([req, saml_config]),
                      ?andReturn({req_json_body, req1}))),
    hoax:mock(cowboy_req,
              ?expect(binding,
                      ?withArgs([ent_name, req1]),
                      ?andReturn({EntName, req2}))),
    hoax:mock(auth_saml_config, [
              ?expect(from_json,
                      ?withArgs([req_json_body, EntName]),
                      ?andReturn(saml_config)),
              ?expect(refresh_metadata,
                      ?withArgs([saml_config]),
                      ?andReturn({error, some_reason}))]),
    hoax:mock(deliv_web_utils,
              ?expect(error_response,
                      ?withArgs([500, internal_server_error, some_reason, req2, state]),
                      ?andReturn({halt, req3, state}))),

    Response = auth_hand_saml_config:from_json(req, state),
    ?assertEqual({halt, req3, state}, Response),
    ?verifyAll.

to_json_returns_404_when_no_config_exists() ->
    EntName = <<"NCC-1701">>,

    hoax:mock(cowboy_req,
              ?expect(binding,
                      ?withArgs([ent_name, req]),
                      ?andReturn({EntName, req1}))),
    hoax:mock(auth_saml_config,
              ?expect(fetch,
                      ?withArgs([EntName]),
                      ?andReturn({error, not_found}))),
    hoax:mock(deliv_web_utils,
              ?expect(error_response,
                      ?withArgs([404, not_found, req1, state]),
                      ?andReturn({body, req2, state}))),

    Actual = auth_hand_saml_config:to_json(req, state),
    ?assertEqual({body, req2, state}, Actual),
    ?verifyAll.

to_json_returns_metadata_info_when_config_exists_with_metadata() ->
    EntName = <<"NCC-1701">>,
    hoax:mock(cowboy_req,
              ?expect(binding,
                      ?withArgs([ent_name, req]),
                      ?andReturn({EntName, req1}))),
    hoax:mock(auth_saml_config, [
              ?expect(fetch,
                      ?withArgs([EntName]),
                      ?andReturn({ok, config_record})),
              ?expect(to_json,
                      ?withArgs([config_record]),
                      ?andReturn(config_json))]),
    hoax:mock(deliv_web_utils,
              ?expect(content,
                      ?withArgs([config_json, req1, state]),
                      ?andReturn({body, req2, state}))),

    Actual = auth_hand_saml_config:to_json(req, state),
    ?assertEqual({body, req2, state}, Actual),
    ?verifyAll.

to_json_returns_200_when_config_exists() ->
    EntName = <<"NCC-1701">>,
    hoax:mock(cowboy_req,
              ?expect(binding,
                      ?withArgs([ent_name, req]),
                      ?andReturn({EntName, req1}))),
    hoax:mock(auth_saml_config, [
              ?expect(fetch,
                      ?withArgs([EntName]),
                      ?andReturn({ok, config_record})),
              ?expect(to_json,
                      ?withArgs([config_record]),
                      ?andReturn(config_json))]),
    hoax:mock(deliv_web_utils,
              ?expect(content,
                      ?withArgs([config_json, req1, state]),
                      ?andReturn({body, req2, state}))),

    Actual = auth_hand_saml_config:to_json(req, state),
    ?assertEqual({body, req2, state}, Actual),
    ?verifyAll.


to_json_returns_500_when_database_error() ->
    EntName = <<"NCC-1701">>,
    hoax:mock(cowboy_req,
              ?expect(binding,
                      ?withArgs([ent_name, req]),
                      ?andReturn({EntName, req1}))),
    hoax:mock(auth_saml_config,
              ?expect(fetch,
                      ?withArgs([EntName]),
                      ?andReturn({error, because_I_said_so}))),
    hoax:mock(deliv_web_utils,
              ?expect(error_response,
                      ?withArgs([500, internal_server_error, req1, state]),
                      ?andReturn({halt, req2, state}))),

    Actual = auth_hand_saml_config:to_json(req, state),
    ?assertEqual({halt, req2, state}, Actual),
    ?verifyAll.

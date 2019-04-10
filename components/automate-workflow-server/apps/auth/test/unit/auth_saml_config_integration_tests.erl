-module(auth_saml_config_integration_tests).

-include("auth_types.hrl").
-include_lib("hoax/include/hoax.hrl").
-include_lib("esaml/include/esaml.hrl").

-compile([export_all]).

auth_saml_config_test_() ->
    [
     hoax:parameterized_fixture(?MODULE, "fetch_", setup, teardown),
     hoax:parameterized_fixture(?MODULE, "upsert_", setup, teardown),
     hoax:parameterized_fixture(?MODULE, "delete_", setup, teardown),
     hoax:parameterized_fixture(?MODULE, "service_provider_",
                                configure_entity_id, unconfigure_entity_id),
     hoax:fixture(?MODULE, "identity_provider_")
    ].

setup() ->
    error_logger:tty(false),
    application:start(gproc),
    eu_database:setup(),
    eu_data:with_enterprise(<<"NCC-1701">>,
        fun(Enterprise) ->
            Enterprise
        end).

teardown(_) ->
    eu_database:teardown(),
    application:stop(gproc),
    error_logger:tty(true).

configure_entity_id(_) ->
    application:set_env(auth, saml_entity_id, "https://saml-for-tests.chef.io").

unconfigure_entity_id(_) ->
    application:set_env(auth, saml_entity_id, undefined).

upsert_auth_saml_config_returns_error_when_bad_enterprise(_Enterprise) ->
    EntName = <<"quantum_enterprise">>,
    SSOLoginUrl = <<"bomb.com/login">>,
    SSOBinding =  <<"HTTP-POST">>,
    IdpUrl = <<"bomb.com">>,
    Cert = <<"secrets">>,
    NameId = <<"urn:oasis:names:tc:SAML:2.0:nameid-format:entity">>,
    ConfigRecord = #saml_config{ent_name = EntName,
                                sso_login_url = SSOLoginUrl,
                                sso_binding = SSOBinding,
                                idp_url = IdpUrl,
                                cert = Cert,
                                name_id = NameId},
    Result = auth_saml_config:upsert(ConfigRecord),
    ?assertEqual({error, {<<"CD003">>,<<"Enterprise not found">>}}, Result).

fetch_when_config_exists_successfully_fetches_upserted_config(Enterprise) ->
    EntName = deliv_enterprise:getval(name, Enterprise),
    SSOLoginUrl = <<"bomb.com/login">>,
    SSOBinding =  <<"HTTP-POST">>,
    IdpUrl = <<"bomb.com">>,
    Cert = <<"secrets">>,
    NameId = <<"urn:oasis:names:tc:SAML:2.0:nameid-format:entity">>,
    DefaultRoles = [<<"observer">>, <<"admin">>],
    ConfigRecord = #saml_config{ent_name = EntName,
                                sso_login_url = SSOLoginUrl,
                                sso_binding = SSOBinding,
                                idp_url = IdpUrl,
                                cert = Cert,
                                name_id = NameId,
                                default_roles = DefaultRoles},
    {ok, UpsertResult} = auth_saml_config:upsert(ConfigRecord),
    {ok, FetchResult} = auth_saml_config:fetch(EntName),
    ?assertEqual(UpsertResult, FetchResult),
    ?assertEqual(DefaultRoles, FetchResult#saml_config.default_roles).

fetch_when_config_exists_successfully_fetches_upserted_config_with_metadata_url_and_xml(Enterprise) ->
    EntName = deliv_enterprise:getval(name, Enterprise),
    MetadataUrl = <<"bomb.com/login">>,
    MetadataXml = <<"<?xml/?>">>,
    NameId = <<"urn:oasis:names:tc:SAML:2.0:nameid-format:entity">>,
    ConfigRecord = #saml_config{ent_name = EntName,
                                metadata_url = MetadataUrl,
                                metadata_xml = MetadataXml,
                                name_id = NameId},
    Result = auth_saml_config:upsert(ConfigRecord),
    ?assertEqual(Result, auth_saml_config:fetch(EntName)).

%% this test is explicitly testing that when the database has a null value for
%% default_roles that we pre-populate the record with a default to continue the
%% default behavior from previous releases
fetch_when_config_exists_with_undefined_default_roles_returns_observer_in_roles(Enterprise) ->
    EntName = deliv_enterprise:getval(name, Enterprise),
    EntId = integer_to_binary(deliv_enterprise:getval(id, Enterprise)),
    SSOLoginUrl = <<"bomb.com/login">>,
    SSOBinding =  <<"HTTP-POST">>,
    IdpUrl = <<"bomb.com">>,
    Cert = <<"secrets">>,
    NameId = <<"urn:oasis:names:tc:SAML:2.0:nameid-format:entity">>,
    PersistedRoles = undefined,
    DefaultRolesDefault = [<<"observer">>],
    ConfigRecord = #saml_config{ent_name = EntName,
                                sso_login_url = SSOLoginUrl,
                                sso_binding = SSOBinding,
                                idp_url = IdpUrl,
                                cert = Cert,
                                name_id = NameId,
                                default_roles = PersistedRoles},

    {ok, UpsertResult} = auth_saml_config:upsert(ConfigRecord),

    ExpectedDBRow = [{<<"default_roles">>,null}],
    DbQuery = <<"SELECT default_roles FROM saml_config WHERE enterprise_id = ", EntId/binary, ";">>,
    ?assertEqual({ok, [ExpectedDBRow]}, sqerl:execute(DbQuery)),

    {ok, FetchResult} = auth_saml_config:fetch(EntName),
    ?assertEqual(UpsertResult, FetchResult),
    ?assertEqual(DefaultRolesDefault, FetchResult#saml_config.default_roles).

fetch_when_no_config_exists_returns_error(Enterprise) ->
    EntName = deliv_enterprise:getval(name, Enterprise),

    Result = auth_saml_config:fetch(EntName),
    ?assertEqual(Result, {error, not_found}).

upsert_when_config_exists_updates_saved_config(Enterprise) ->
    EntName = deliv_enterprise:getval(name, Enterprise),
    SSOLoginUrl = <<"bomb.com/login">>,
    SSOBinding =  <<"HTTP-POST">>,
    IdpUrl = <<"bomb.com">>,
    Cert = <<"secrets">>,
    NameId = <<"urn:oasis:names:tc:SAML:2.0:nameid-format:entity">>,
    NewCert = <<"public">>,
    DefaultRoles = [<<"observer">>, <<"admin">>],
    ConfigRecord = #saml_config{ent_name = EntName,
                                sso_login_url = SSOLoginUrl,
                                sso_binding = SSOBinding,
                                idp_url = IdpUrl,
                                cert = Cert,
                                name_id = NameId,
                                default_roles = DefaultRoles},
    auth_saml_config:upsert(ConfigRecord),
    NewConfig = ConfigRecord#saml_config{cert = NewCert},
    {ok, Result} = auth_saml_config:upsert(NewConfig),
    ?assertEqual(NewConfig, Result),
    ?assertEqual({ok, NewConfig}, auth_saml_config:fetch(EntName)).

upsert_with_metadata_url_and_xml_when_manual_config_exists_updates_config(Enterprise) ->
    EntName = deliv_enterprise:getval(name, Enterprise),
    MetadataUrl = <<"idp.com/metadata">>,
    MetadataXml = <<"<? xml ?>">>,
    SSOLoginUrl = <<"bomb.com/login">>,
    SSOBinding =  <<"HTTP-POST">>,
    IdpUrl = <<"bomb.com">>,
    Cert = <<"secrets">>,
    NameId = <<"urn:oasis:names:tc:SAML:2.0:nameid-format:entity">>,
    ConfigRecord = #saml_config{ent_name = EntName,
                                sso_login_url = SSOLoginUrl,
                                sso_binding = SSOBinding,
                                idp_url = IdpUrl,
                                cert = Cert,
                                name_id = NameId},
    NewConfigRecord = #saml_config{ent_name = EntName,
                                   metadata_url = MetadataUrl,
                                   metadata_xml = MetadataXml,
                                   name_id = NameId},
    {ok, _} = auth_saml_config:upsert(ConfigRecord),
    {ok, Result} = auth_saml_config:upsert(NewConfigRecord),
    ?assertEqual(undefined, Result#saml_config.sso_login_url),
    ?assertEqual(undefined, Result#saml_config.sso_binding),
    ?assertEqual(undefined, Result#saml_config.idp_url),
    ?assertEqual(undefined, Result#saml_config.cert),
    ?assertEqual(NameId, Result#saml_config.name_id),
    ?assertEqual(MetadataUrl, Result#saml_config.metadata_url),
    ?assertEqual(MetadataXml, Result#saml_config.metadata_xml).

upsert_with_manual_config_when_metadata_url_and_xml_exists_updates_config(Enterprise) ->
    EntName = deliv_enterprise:getval(name, Enterprise),
    MetadataUrl = <<"idp.com/metadata">>,
    MetadataXml = <<"<? xml ?>">>,
    SSOLoginUrl = <<"bomb.com/login">>,
    SSOBinding =  <<"HTTP-POST">>,
    IdpUrl = <<"bomb.com">>,
    Cert = <<"secrets">>,
    NameId = <<"urn:oasis:names:tc:SAML:2.0:nameid-format:entity">>,
    ConfigRecord = #saml_config{ent_name = EntName,
                                metadata_url = MetadataUrl,
                                metadata_xml = MetadataXml,
                                name_id = NameId},
    NewConfigRecord = #saml_config{ent_name = EntName,
                                   sso_login_url = SSOLoginUrl,
                                   sso_binding = SSOBinding,
                                   idp_url = IdpUrl,
                                   cert = Cert,
                                   name_id = NameId},
    {ok, _} = auth_saml_config:upsert(ConfigRecord),
    {ok, Result} = auth_saml_config:upsert(NewConfigRecord),
    ?assertEqual(SSOLoginUrl, Result#saml_config.sso_login_url),
    ?assertEqual(SSOBinding, Result#saml_config.sso_binding),
    ?assertEqual(IdpUrl, Result#saml_config.idp_url),
    ?assertEqual(Cert, Result#saml_config.cert),
    ?assertEqual(NameId, Result#saml_config.name_id),
    ?assertEqual(undefined, Result#saml_config.metadata_url),
    ?assertEqual(undefined, Result#saml_config.metadata_xml).

upsert_when_no_config_exists_creates_config(Enterprise) ->
    EntName = deliv_enterprise:getval(name, Enterprise),
    SSOLoginUrl = <<"bomb.com/login">>,
    SSOBinding =  <<"HTTP-POST">>,
    IdpUrl = <<"bomb.com">>,
    Cert = <<"secrets">>,
    NameId = <<"urn:oasis:names:tc:SAML:2.0:nameid-format:entity">>,
    DefaultRoles = [<<"observer">>, <<"reviewer">>],
    ConfigRecord = #saml_config{ent_name = EntName,
                                sso_login_url = SSOLoginUrl,
                                sso_binding = SSOBinding,
                                idp_url = IdpUrl,
                                cert = Cert,
                                name_id = NameId,
                                default_roles = DefaultRoles},
    {ok, Result} = auth_saml_config:upsert(ConfigRecord),
    ?assertEqual(SSOLoginUrl, Result#saml_config.sso_login_url),
    ?assertEqual(SSOBinding, Result#saml_config.sso_binding),
    ?assertEqual(IdpUrl, Result#saml_config.idp_url),
    ?assertEqual(Cert, Result#saml_config.cert),
    ?assertEqual(NameId, Result#saml_config.name_id),
    ?assertEqual(DefaultRoles, Result#saml_config.default_roles),
    ?assertEqual(undefined, Result#saml_config.metadata_url),
    ?assertEqual(undefined, Result#saml_config.metadata_xml).

upsert_when_no_config_exists_with_empty_default_roles_returns_observer_in_roles(Enterprise) ->
    EntName = deliv_enterprise:getval(name, Enterprise),
    EntId = integer_to_binary(deliv_enterprise:getval(id, Enterprise)),
    SSOLoginUrl = <<"bomb.com/login">>,
    SSOBinding =  <<"HTTP-POST">>,
    IdpUrl = <<"bomb.com">>,
    Cert = <<"secrets">>,
    NameId = <<"urn:oasis:names:tc:SAML:2.0:nameid-format:entity">>,
    DefaultRoles = undefined,
    DefaultRolesDefault = [<<"observer">>],
    ConfigRecord = #saml_config{ent_name = EntName,
                                sso_login_url = SSOLoginUrl,
                                sso_binding = SSOBinding,
                                idp_url = IdpUrl,
                                cert = Cert,
                                name_id = NameId,
                                default_roles = DefaultRoles},
    {ok, Result} = auth_saml_config:upsert(ConfigRecord),

    ExpectedDBRow = [{<<"default_roles">>,null}],
    DbQuery = <<"SELECT default_roles FROM saml_config WHERE enterprise_id = ", EntId/binary, ";">>,
    ?assertEqual({ok, [ExpectedDBRow]}, sqerl:execute(DbQuery)),

    ?assertEqual(SSOLoginUrl, Result#saml_config.sso_login_url),
    ?assertEqual(SSOBinding, Result#saml_config.sso_binding),
    ?assertEqual(IdpUrl, Result#saml_config.idp_url),
    ?assertEqual(Cert, Result#saml_config.cert),
    ?assertEqual(NameId, Result#saml_config.name_id),
    ?assertEqual(DefaultRolesDefault, Result#saml_config.default_roles),
    ?assertEqual(undefined, Result#saml_config.metadata_url),
    ?assertEqual(undefined, Result#saml_config.metadata_xml).

delete_when_no_config_exists_returns_ok(Enterprise) ->
    EntName = deliv_enterprise:getval(name, Enterprise),
    Result = auth_saml_config:delete(EntName),
    ?assertEqual(ok, Result).

delete_when_config_exists_successfully_deletes_config_and_fetch_returns_not_found(Enterprise) ->
    EntName = deliv_enterprise:getval(name, Enterprise),
    SSOLoginUrl = <<"bomb.com/login">>,
    SSOBinding =  <<"HTTP-POST">>,
    IdpUrl = <<"bomb.com">>,
    Cert = <<"secrets">>,
    NameId = <<"urn:oasis:names:tc:SAML:2.0:nameid-format:entity">>,
    ConfigRecord = #saml_config{ent_name = EntName,
                                sso_login_url = SSOLoginUrl,
                                sso_binding = SSOBinding,
                                idp_url = IdpUrl,
                                cert = Cert,
                                name_id = NameId},
    auth_saml_config:upsert(ConfigRecord),
    Result1 = auth_saml_config:delete(EntName),
    ?assertEqual(ok, Result1),
    Result2 = auth_saml_config:fetch(EntName),
    ?assertEqual({error, not_found}, Result2).

service_provider_when_esaml_sp_setup_success_returns_esaml_sp_record() ->
    Cert = <<"MIIDPDCCAiQCCQDydJgOlszqbzANBgkqhkiG9w0BAQUFADBgMQswCQYDVQQGEwJVUzETMBEGA1UECBMKQ2FsaWZvcm5pYTEWMBQGA1UEBxMNU2FuIEZyYW5jaXNjbzEQMA4GA1UEChMHSmFua3lDbzESMBAGA1UEAxMJbG9jYWxob3N0MB4XDTE0MDMxMjE5NDYzM1oXDTI3MTExOTE5NDYzM1owYDELMAkGA1UEBhMCVVMxEzARBgNVBAgTCkNhbGlmb3JuaWExFjAUBgNVBAcTDVNhbiBGcmFuY2lzY28xEDAOBgNVBAoTB0phbmt5Q28xEjAQBgNVBAMTCWxvY2FsaG9zdDCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBAMGvJpRTTasRUSPqcbqCG+ZnTAurnu0vVpIG9lzExnh11o/BGmzu7lB+yLHcEdwrKBBmpepDBPCYxpVajvuEhZdKFx/Fdy6j5mH3rrW0Bh/zd36CoUNjbbhHyTjeM7FN2yF3u9lcyubuvOzr3B3gX66IwJlU46+wzcQVhSOlMk2tXR+fIKQExFrOuK9tbX3JIBUqItpI+HnAow509CnM134svw8PTFLkR6/CcMqnDfDK1m993PyoC1Y+N4X9XkhSmEQoAlAHPI5LHrvuujM13nvtoVYvKYoj7ScgumkpWNEvX652LfXOnKYlkB8ZybuxmFfIkzedQrbJsyOhfL03cMECAwEAATANBgkqhkiG9w0BAQUFAAOCAQEAeHwzqwnzGEkxjzSD47imXaTqtYyETZow7XwBc0ZaFS50qRFJUgKTAmKS1xQBP/qHpStsROT35DUxJAE6NY1Kbq3ZbCuhGoSlY0L7VzVT5tpu4EY8+Dq/u2EjRmmhoL7UkskvIZ2n1DdERtd+YUMTeqYl9co43csZwDno/IKomeN5qaPc39IZjikJ+nUC6kPFKeu/3j9rgHNlRtocI6S1FdtFz9OZMQlpr0JbUt2T3xS/YoQJn6coDmJL5GTiiKM6cOe+Ur1VwzS1JEDbSS2TWWhzq8ojLdrotYLGd9JOsoQhElmz+tMfCFQUFLExinPAyy7YHlSiVX13QH2XTu/iQQ==">>,
    CorrectFingerprints = [{sha256,
                            <<162,171,107,192,92,182,162,180,12,20,49,144,
                              31,147,179,78,223,153,108,146,96,125,173,165,
                              153,40,254,185,181,196,45,166>>}],
    EntName = <<"testEnt">>,
    ConsumeUri = "https://delivery.com/api/v0/e/testEnt/saml/consume",
    EntityId = "https://saml.chef.io",
    ExpectedServiceProvider = #esaml_sp{consume_uri = ConsumeUri,
                                        metadata_uri = EntityId,
                                        idp_signs_envelopes = false,
                                        idp_signs_assertions = true,
                                        trusted_fingerprints = CorrectFingerprints
                                       },
    application:set_env(delivery, api_proto, "https"),
    application:set_env(delivery, hostname, "delivery.com"),

    Actual = auth_saml_config:service_provider(EntName, Cert),
    ?assertEqual({ok, ExpectedServiceProvider}, Actual),
    ?verifyAll.

identity_provider_when_azure_metadata_xml_is_present_builds_idp_record_from_xml() ->
    MetadataFilePath = app_test_helpers:project_path(?MODULE, "test/unit/data/azure_metadata.xml"),
    {ok, MetadataXML} = file:read_file(MetadataFilePath),
    EntName = <<"testEnt">>,
    MetadataUrl = <<"www.metadata.com">>,
    SSOBinding = <<"HTTP-Redirect">>,
    NameIdFormat = <<"default">>,
    ExpectedCertificates = [
             <<"MIIC4jCCAcqgAwIBAgIQQNXrmzhLN4VGlUXDYCRT3zANBgkqhkiG9w0BAQsFADAtMSswKQYDVQQDEyJhY2NvdW50cy5hY2Nlc3Njb250cm9sLndpbmRvd3MubmV0MB4XDTE0MTAyODAwMDAwMFoXDTE2MTAyNzAwMDAwMFowLTErMCkGA1UEAxMiYWNjb3VudHMuYWNjZXNzY29udHJvbC53aW5kb3dzLm5ldDCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBALyKs/uPhEf7zVizjfcr/ISGFe9+yUOqwpel38zgutvLHmFD39E2hpPdQhcXn4c4dt1fU5KvkbcDdVbP8+e4TvNpJMy/nEB2V92zCQ/hhBjilwhF1ETe1TMmVjALs0KFvbxW9ZN3EdUVvxFvz/gvG29nQhl4QWKj3x8opr89lmq14Z7T0mzOV8kub+cgsOU/1bsKqrIqN1fMKKFhjKaetctdjYTfGzVQ0AJAzzbtg0/Q1wdYNAnhSDafygEv6kNiquk0r0RyasUUevEXs2LY3vSgKsKseI8ZZlQEMtE9/k/iAG7JNcEbVg53YTurNTrPnXJOU88mf3TToX14HpYsS1ECAwEAATANBgkqhkiG9w0BAQsFAAOCAQEAfolx45w0i8CdAUjjeAaYdhG9+NDHxop0UvNOqlGqYJexqPLuvX8iyUaYxNGzZxFgGI3GpKfmQP2JQWQ1E5JtY/n8iNLOKRMwqkuxSCKJxZJq4Sl/m/Yv7TS1P5LNgAj8QLCypxsWrTAmq2HSpkeSk4JBtsYxX6uhbGM/K1sEktKybVTHu22/7TmRqWTmOUy9wQvMjJb2IXdMGLG3hVntN/WWcs5w8vbt1i8Kk6o19W2MjZ95JaECKjBDYRlhG1KmSBtrsKsCBQoBzwH/rXfksTO9JoUYLXiW0IppB7DhNH4PJ5hZI91R8rR0H3/bKkLSuDaKLWSqMhozdhXsIIKvJQ==">>,
             <<"MIIC4jCCAcqgAwIBAgIQfQ29fkGSsb1J8n2KueDFtDANBgkqhkiG9w0BAQsFADAtMSswKQYDVQQDEyJhY2NvdW50cy5hY2Nlc3Njb250cm9sLndpbmRvd3MubmV0MB4XDTE2MDQxNzAwMDAwMFoXDTE4MDQxNzAwMDAwMFowLTErMCkGA1UEAxMiYWNjb3VudHMuYWNjZXNzY29udHJvbC53aW5kb3dzLm5ldDCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBAL23Ba49fdxpus3qOXtv8ueCcePbCIEnL/tiTvp+jcTakGilZzJB3M/ktY9hX6RZ4KLcBjM2SClQmNUivEimTBX0U1N8L06GSE8H91tUKup/ofmm6qciU2qiHH4QNHepBADOTbEACoX78O363tUInJlPS1lVlGAGsi5okV+qN7ZLSauh+fKVM07cfw9A6a58es+bFvrojIqS1264GJjns+4baJCVYA4PMPsgxQsWTaOylbnlJC5MYTY2BpBn57dfLO2VtN+lqE5nWkJluAgoX/6OEyxOVchqWFpuyP/p1feQQb8Jc6JFVSs73in95eVFN3Oj5BsvgQdxPwoahZurD1sCAwEAATANBgkqhkiG9w0BAQsFAAOCAQEAe5RxtMLU2i4/vN1YacncR3GkOlbRv82rll9cd5mtVmokAw7kwbFBFNo2vIVkun+n+VdJf+QRzmHGm3ABtKwz3DPr78y0qdVFA3h9P60hd3wqu2k5/Q8s9j1Kq3u9TIEoHlGJqNzjqO7khX6VcJ6BRLzoefBYavqoDSgJ3mkkYCNqTV2ZxDNks3obPg4yUkh5flULH14TqlFIOhXbsd775aPuMT+/tyqcc6xohU5NyYA63KtWG1BLDuF4LEF84oNPcY9i0n6IphEGgz20H7YcLRNjU55pDbWGdjE4X8ANb23kAc75RZn9EY4qYCiqeIAg3qEVKLnLUx0fNKMHmuedjg==">>
            ],
    Config = auth_saml_config:fromlist([ {metadata_url, MetadataUrl},
                                         {metadata_xml, MetadataXML},
                                         {sso_binding, SSOBinding},
                                         {name_id, NameIdFormat}]),
    ExpectedEntityId = "https://sts.windows.net/1b218ca8-3694-4fcb-ac12-d2112c657830/",
    ExpectedLoginLocation = "https://login.microsoftonline.com/1b218ca8-3694-4fcb-ac12-d2112c657830/saml2",
    hoax:mock(deliv_db,
              ?expect(qfetch,
                      ?withArgs([auth_saml_config, fetch_by_enterprise_name, [EntName]]),
                      ?andReturn([Config]))),

    {ok, IdPMetadata, ActualSSOBinding, ActualNameIdFormat} = auth_saml_config:identity_provider(EntName),
    ?assertEqual(ExpectedEntityId, IdPMetadata#esaml_idp_metadata.entity_id),
    ?assertEqual(ExpectedLoginLocation, IdPMetadata#esaml_idp_metadata.login_location_redirect),
    ?assertEqual(NameIdFormat, ActualNameIdFormat),
    ?assertEqual(SSOBinding, ActualSSOBinding),
    ?assertEqual(lists:map(fun base64:decode/1, ExpectedCertificates), IdPMetadata#esaml_idp_metadata.certificates),
    ?verifyAll.

identity_provider_when_okta_metadata_xml_is_present_builds_idp_record_from_xml() ->
    MetadataFilePath = app_test_helpers:project_path(?MODULE, "test/unit/data/okta_metadata.xml"),
    {ok, MetadataXML} = file:read_file(MetadataFilePath),
    EntName = <<"testEnt">>,
    MetadataUrl = <<"www.metadata.com">>,
    SSOBinding = <<"HTTP-Redirect">>,
    NameIdFormat = <<"default">>,
    Cert = <<"MIIDpDCCAoygAwIBAgIGAVSraG50MA0GCSqGSIb3DQEBBQUAMIGSMQswCQYDVQQGEwJVUzETMBEGA1UECAwKQ2FsaWZvcm5pYTEWMBQGA1UEBwwNU2FuIEZyYW5jaXNjbzENMAsGA1UECgwET2t0YTEUMBIGA1UECwwLU1NPUHJvdmlkZXIxEzARBgNVBAMMCmRldi05MTA0OTYxHDAaBgkqhkiG9w0BCQEWDWluZm9Ab2t0YS5jb20wHhcNMTYwNTEzMTgzNjA4WhcNMjYwNTEzMTgzNzA4WjCBkjELMAkGA1UEBhMCVVMxEzARBgNVBAgMCkNhbGlmb3JuaWExFjAUBgNVBAcMDVNhbiBGcmFuY2lzY28xDTALBgNVBAoMBE9rdGExFDASBgNVBAsMC1NTT1Byb3ZpZGVyMRMwEQYDVQQDDApkZXYtOTEwNDk2MRwwGgYJKoZIhvcNAQkBFg1pbmZvQG9rdGEuY29tMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEA3apvyq3Ub0qXCGoeAzy6FgnJdL5bje9WDEAno4nScayedoAHgFMNtKano2OIB+NP4bzVcbo1iiGJK1VW4gpT0Y4bpPVngn6IpSwM9f1j/fNvTvO+PvTlziQAUeeCwoBNXcoEDEacU1x1OL6a3YI/RPvst5hoRfrMkGI220pvwbbY5J3AyeIULQ+d2qtN0LqnmYUxfZ9SS6ADm4VO/qKUKzyBogHt1fZuH5QaEjoCOadKQP9ug1On/lSQ8nExYJBU9n7X8oEkqxPGn1dr59Zul3rmItklacDs9M8/pmetiZzwnXM8sWoKzYYLnwhuS213GmH6jD2/e4/2CXntBNBIDwIDAQABMA0GCSqGSIb3DQEBBQUAA4IBAQBLw1r38gWoqhxBtyeTlUwzK3iFdni6QyEZTK4A87d540y47l+r/55UGCqV2Y2qnn52UEmge0wq4LBLhcZh4XESFILsIkJQqBIWXz6VD35Tu8uqgD6FcGuy/uuTkMyx32+a2KOJBwi8wVMe/O9H8l+mIoLDRhPz2CqYNrSQGjy6c7BDURbHNqYemGyYYVaKtF87b6hK98d3MxmnSoTXVHsoI6iNq7475yZaDfwLc6YEtKD07rh/537nVWK2YKswxalv+8eQgfxLxYtbyxXNfYiHrp+6GyikFuHNLReweRR1gPamGY5xPmskVO2+XznKZKDxpegJZZyv2YRZHTB3HKzL">>,
    Config = auth_saml_config:fromlist([ {metadata_url, MetadataUrl},
                                         {metadata_xml, MetadataXML},
                                         {sso_binding, SSOBinding},
                                         {name_id, NameIdFormat}]),
    ExpectedEntityId = "http://www.okta.com/exk6cmua3ybZ8G1sz0h7",
    ExpectedLoginLocation = "https://dev-910496.oktapreview.com/app/chefsoftwareincdev910496_deliverydevelopment_1/exk6cmua3ybZ8G1sz0h7/sso/saml",
    ExpectedCertificate = Cert,
    hoax:mock(deliv_db,
              ?expect(qfetch,
                      ?withArgs([auth_saml_config, fetch_by_enterprise_name, [EntName]]),
                      ?andReturn([Config]))),

    {ok, IdPMetadata, ActualSSOBinding, ActualNameIdFormat} = auth_saml_config:identity_provider(EntName),
    ?assertEqual(ExpectedEntityId, IdPMetadata#esaml_idp_metadata.entity_id),
    ?assertEqual(ExpectedLoginLocation, IdPMetadata#esaml_idp_metadata.login_location_redirect),
    ?assertEqual(NameIdFormat, ActualNameIdFormat),
    ?assertEqual(SSOBinding, ActualSSOBinding),
    ?assertEqual([base64:decode(ExpectedCertificate)], IdPMetadata#esaml_idp_metadata.certificates),
    ?verifyAll.

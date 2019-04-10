-module(auth_saml_utils_tests).

-include_lib("hoax/include/hoax.hrl").

-compile(export_all).

hoax_fixture_test_() ->
    [
     hoax:fixture(?MODULE, "login_url_"),
     hoax:fixture(?MODULE, "calculate_fingerprint_"),
     hoax:fixture(?MODULE, "encode_relaystate_"),
     hoax:fixture(?MODULE, "decode_relaystate_"),
     hoax:fixture(?MODULE, "redirect_binding_")
    ].

login_url_with_error_param_returns_target_with_error_message() ->
    TestMessage = <<"TestMessage">>,
    TargetMessageBase64 = base64:encode(TestMessage),
    EntName = <<"TestEnt">>,
    Url = <<"http://delivery/login?error=">>,
    ExpectedTarget = <<Url/binary, TargetMessageBase64/binary>>,
    hoax:mock(deliv_web_utils,
              ?expect(make_web_url_for_login,
                      ?withArgs([EntName]),
                      ?andReturn(<<"http://delivery/login">>))),

    Target = auth_saml_utils:login_url_with_error_param(EntName, TestMessage),
    ?assertEqual(Target, ExpectedTarget),
    ?verifyAll.

calculate_fingerprint_when_given_base64_encoded_data_returns_correct_fingerprint() ->
    Cert = <<"MIIDPDCCAiQCCQDydJgOlszqbzANBgkqhkiG9w0BAQUFADBgMQswCQYDVQQGEwJVUzETMBEGA1UECBMKQ2FsaWZvcm5pYTEWMBQGA1UEBxMNU2FuIEZyYW5jaXNjbzEQMA4GA1UEChMHSmFua3lDbzESMBAGA1UEAxMJbG9jYWxob3N0MB4XDTE0MDMxMjE5NDYzM1oXDTI3MTExOTE5NDYzM1owYDELMAkGA1UEBhMCVVMxEzARBgNVBAgTCkNhbGlmb3JuaWExFjAUBgNVBAcTDVNhbiBGcmFuY2lzY28xEDAOBgNVBAoTB0phbmt5Q28xEjAQBgNVBAMTCWxvY2FsaG9zdDCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBAMGvJpRTTasRUSPqcbqCG+ZnTAurnu0vVpIG9lzExnh11o/BGmzu7lB+yLHcEdwrKBBmpepDBPCYxpVajvuEhZdKFx/Fdy6j5mH3rrW0Bh/zd36CoUNjbbhHyTjeM7FN2yF3u9lcyubuvOzr3B3gX66IwJlU46+wzcQVhSOlMk2tXR+fIKQExFrOuK9tbX3JIBUqItpI+HnAow509CnM134svw8PTFLkR6/CcMqnDfDK1m993PyoC1Y+N4X9XkhSmEQoAlAHPI5LHrvuujM13nvtoVYvKYoj7ScgumkpWNEvX652LfXOnKYlkB8ZybuxmFfIkzedQrbJsyOhfL03cMECAwEAATANBgkqhkiG9w0BAQUFAAOCAQEAeHwzqwnzGEkxjzSD47imXaTqtYyETZow7XwBc0ZaFS50qRFJUgKTAmKS1xQBP/qHpStsROT35DUxJAE6NY1Kbq3ZbCuhGoSlY0L7VzVT5tpu4EY8+Dq/u2EjRmmhoL7UkskvIZ2n1DdERtd+YUMTeqYl9co43csZwDno/IKomeN5qaPc39IZjikJ+nUC6kPFKeu/3j9rgHNlRtocI6S1FdtFz9OZMQlpr0JbUt2T3xS/YoQJn6coDmJL5GTiiKM6cOe+Ur1VwzS1JEDbSS2TWWhzq8ojLdrotYLGd9JOsoQhElmz+tMfCFQUFLExinPAyy7YHlSiVX13QH2XTu/iQQ==">>,
    CorrectFingerprint = "SHA256:oqtrwFy2orQMFDGQH5OzTt+ZbJJgfa2lmSj+ubXELaY=",

    ?assertEqual(CorrectFingerprint, auth_saml_utils:calculate_fingerprint(Cert)).

calculate_fingerprint_when_given_binary_data_returns_correct_fingerprint() ->
    CertBin = <<48,130,3,60,48,130,2,36,2,9,0,242,116,152,14,150,204,234,111,48,13,6,9,42,134,72,134,247,13,1,1,5,5,0,48,96,49,11,48,9,6,3,85,4,6,19,2,85,83,49,19,48,17,6,3,85,4,8,19,10,67,97,108,105,102,111,114,110,105,97,49,22,48,20,6,3,85,4,7,19,13,83,97,110,32,70,114,97,110,99,105,115,99,111,49,16,48,14,6,3,85,4,10,19,7,74,97,110,107,121,67,111,49,18,48,16,6,3,85,4,3,19,9,108,111,99,97,108,104,111,115,116,48,30,23,13,49,52,48,51,49,50,49,57,52,54,51,51,90,23,13,50,55,49,49,49,57,49,57,52,54,51,51,90,48,96,49,11,48,9,6,3,85,4,6,19,2,85,83,49,19,48,17,6,3,85,4,8,19,10,67,97,108,105,102,111,114,110,105,97,49,22,48,20,6,3,85,4,7,19,13,83,97,110,32,70,114,97,110,99,105,115,99,111,49,16,48,14,6,3,85,4,10,19,7,74,97,110,107,121,67,111,49,18,48,16,6,3,85,4,3,19,9,108,111,99,97,108,104,111,115,116,48,130,1,34,48,13,6,9,42,134,72,134,247,13,1,1,1,5,0,3,130,1,15,0,48,130,1,10,2,130,1,1,0,193,175,38,148,83,77,171,17,81,35,234,113,186,130,27,230,103,76,11,171,158,237,47,86,146,6,246,92,196,198,120,117,214,143,193,26,108,238,238,80,126,200,177,220,17,220,43,40,16,102,165,234,67,4,240,152,198,149,90,142,251,132,133,151,74,23,31,197,119,46,163,230,97,247,174,181,180,6,31,243,119,126,130,161,67,99,109,184,71,201,56,222,51,177,77,219,33,119,187,217,92,202,230,238,188,236,235,220,29,224,95,174,136,192,153,84,227,175,176,205,196,21,133,35,165,50,77,173,93,31,159,32,164,4,196,90,206,184,175,109,109,125,201,32,21,42,34,218,72,248,121,192,163,14,116,244,41,204,215,126,44,191,15,15,76,82,228,71,175,194,112,202,167,13,240,202,214,111,125,220,252,168,11,86,62,55,133,253,94,72,82,152,68,40,2,80,7,60,142,75,30,187,238,186,51,53,222,123,237,161,86,47,41,138,35,237,39,32,186,105,41,88,209,47,95,174,118,45,245,206,156,166,37,144,31,25,201,187,177,152,87,200,147,55,157,66,182,201,179,35,161,124,189,55,112,193,2,3,1,0,1,48,13,6,9,42,134,72,134,247,13,1,1,5,5,0,3,130,1,1,0,120,124,51,171,9,243,24,73,49,143,52,131,227,184,166,93,164,234,181,140,132,77,154,48,237,124,1,115,70,90,21,46,116,169,17,73,82,2,147,2,98,146,215,20,1,63,250,135,165,43,108,68,228,247,228,53,49,36,1,58,53,141,74,110,173,217,108,43,161,26,132,165,99,66,251,87,53,83,230,218,110,224,70,60,248,58,191,187,97,35,70,105,161,160,190,212,146,201,47,33,157,167,212,55,68,70,215,126,97,67,19,122,166,37,245,202,56,221,203,25,192,57,232,252,130,168,153,227,121,169,163,220,223,210,25,142,41,9,250,117,2,234,67,197,41,235,191,222,63,107,128,115,101,70,218,28,35,164,181,21,219,69,207,211,153,49,9,105,175,66,91,82,221,147,223,20,191,98,132,9,159,167,40,14,98,75,228,100,226,136,163,58,112,231,190,82,189,85,195,52,181,36,64,219,73,45,147,89,104,115,171,202,35,45,218,232,181,130,198,119,210,78,178,132,33,18,89,179,250,211,31,8,84,20,20,177,49,138,115,192,203,46,216,30,84,162,85,125,119,64,125,151,78,239,226,65>>,
    CorrectFingerprint = "SHA256:oqtrwFy2orQMFDGQH5OzTt+ZbJJgfa2lmSj+ubXELaY=",

    ?assertEqual(CorrectFingerprint, auth_saml_utils:calculate_fingerprint(CertBin)).

encode_relaystate_when_given_erlang_term_returns_base64_encoded_binary_terms() ->
    PropList = [{return_url, <<"some_url">>}],
    ?assertEqual(<<"g2wAAAABaAJkAApyZXR1cm5fdXJsbQAAAAhzb21lX3VybGo=">>,
                 auth_saml_utils:encode_relaystate(PropList)).

decode_relaystate_when_given_base64_encoded_binary_terms_returns_decoded_terms() ->
    RelayState = <<"g2wAAAABaAJkAApyZXR1cm5fdXJsbQAAAAhzb21lX3VybGo=">>,
    ?assertEqual({ok, [{return_url, <<"some_url">>}]},
                 auth_saml_utils:decode_relaystate(RelayState)).

decode_relaystate_when_given_gibberish_returns_error() ->
    RelayState = <<"thisisnotbase64">>,
    ?assertEqual({error, badarg}, auth_saml_utils:decode_relaystate(RelayState)).

decode_relaystate_when_given_an_atom_returns_error() ->
    RelayState = <<"g2QAA2Zvbw==">>, % foo
    ?assertEqual({error, badarg}, auth_saml_utils:decode_relaystate(RelayState)).

decode_relaystate_when_given_no_proper_proplist_returns_error() ->
    RelayState = base64:encode(erlang:term_to_binary([{foo, false}, {bar, very, false}])),
    ?assertEqual({error, badarg}, auth_saml_utils:decode_relaystate(RelayState)).

decode_relaystate_when_given_no_proper_proplist_because_key_not_atom_returns_error() ->
    RelayState = base64:encode(erlang:term_to_binary([{[foo], false}])),
    ?assertEqual({error, badarg}, auth_saml_utils:decode_relaystate(RelayState)).

redirect_binding_with_default_name_id_calls_esaml_with_undefined_name_id_policy() ->
    SSoLoginUri = <<"bomb.com">>,
    NameIdFormat = <<"default">>,
    TrackingFun = fun auth_saml_assertion_timer:generate_and_track_unique_id/0,
    hoax:mock(esaml_sp,
              ?expect(generate_authn_request,
                      ?withArgs([SSoLoginUri, undefined, TrackingFun, service_provider]),
                      ?andReturn(xml))),
    hoax:mock(esaml_binding,
              ?expect(encode_http_redirect,
                      ?withArgs([SSoLoginUri, xml, relay_state]),
                      ?andReturn(redirect_target))),

    RedirectTarget = auth_saml_utils:redirect_binding(service_provider, SSoLoginUri, relay_state, NameIdFormat),
    % Verifying the correct XML is the important thing, which is done with the hoax mock
    ?assertEqual(redirect_target, RedirectTarget),
    ?verifyAll.

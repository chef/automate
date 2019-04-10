%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
-module(deliv_ssl_verify_functions_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("deliv_types.hrl").

-compile(export_all).

%% These tests are really to ensure we don't regress our certificate validation
%% policy.
verify_selfsigned_cert_test_() ->
  hoax:fixture(?MODULE, "verify_selfsigned_cert").

verify_selfsigned_cert_returns_valid_when_cert_is_valid() ->
    Cert = certificate,
    CertFilePath = "CA_Certs/file/path",
    Expected = {valid, CertFilePath},
    ?assertEqual(Expected, deliv_ssl_verify_functions:verify_selfsigned_cert(Cert, valid, CertFilePath)),
    ?verifyAll.

verify_selfsigned_cert_returns_valid_peer_when_cert_is_valid() ->
    Cert = certificate,
    CertFilePath = "CA_Certs/file/path",
    Expected = {valid, CertFilePath},
    ?assertEqual(Expected, deliv_ssl_verify_functions:verify_selfsigned_cert(Cert, valid_peer, CertFilePath)),
    ?verifyAll.

verify_selfsigned_cert_returns_bad_cert_untrusted_selfsigned_peer_when_cert_not_in_trust_store() ->
    Cert = certificate,
    CertFilePath = "CA_Certs/file/path",
    hoax:mock(deliv_ssl_certificates, [
              ?expect(is_trusted_cert,
                      ?withArgs([Cert, CertFilePath]),
                      ?andReturn(false))]),

    Expected = {bad_cert, untrusted_selfsigned_peer},
    ?assertEqual(Expected, deliv_ssl_verify_functions:verify_selfsigned_cert(Cert, {bad_cert, selfsigned_peer}, CertFilePath)),
    ?verifyAll.

verify_selfsigned_cert_returns_valid_when_cert_in_trust_store() ->
    Cert = certificate,
    CertFilePath = "CA_Certs/file/path",
    hoax:mock(deliv_ssl_certificates, [
              ?expect(is_trusted_cert,
                      ?withArgs([Cert, CertFilePath]),
                      ?andReturn(true))]),

    Expected = {valid, CertFilePath},
    ?assertEqual(Expected, deliv_ssl_verify_functions:verify_selfsigned_cert(Cert, {bad_cert, selfsigned_peer}, CertFilePath)),
    ?verifyAll.

verify_selfsigned_cert_returns_fail_when_bad_cert_any() ->
    Expected = {fail, {bad_cert, any_error}},
    ?assertEqual(Expected, deliv_ssl_verify_functions:verify_selfsigned_cert(cert, {bad_cert, any_error}, user_state)).

verify_selfsigned_cert_returns_fail_when_revoked_reason() ->
    Expected = {fail, {revoked, revocation_reason}},
    ?assertEqual(Expected, deliv_ssl_verify_functions:verify_selfsigned_cert(cert, {revoked, revocation_reason}, user_state)).

verify_selfsigned_cert_returns_unknown_when_extension_any() ->
    Expected = {unknown, user_state},
    ?assertEqual(Expected, deliv_ssl_verify_functions:verify_selfsigned_cert(cert, {extension, any_extension}, user_state)).

verify_selfsigned_cert_returns_error_when_no_function_head() ->
    ?assertException(error, function_clause, deliv_ssl_verify_functions:verify_selfsigned_cert(invalid_clause, invalid_clause, invalid_clause)).

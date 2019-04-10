-module(deliv_ssl_certificates_tests).

-compile([export_all]).

-include_lib("hoax/include/hoax.hrl").

is_trusted_cert_test_() ->
    hoax:fixture(?MODULE, "is_trusted_cert_").

is_trusted_cert_returns_true_when_certificate_in_trust_store() ->
    CaCertFilePath = app_test_helpers:project_path(?MODULE, "test/unit/data/certificates/ca-certificates.crt"),

    TrustedCertFilePath = app_test_helpers:project_path(?MODULE, "test/unit/data/certificates/trusted.crt"),

    TrustedCert = read_single_cert_from_file(TrustedCertFilePath),

    ?assert(deliv_ssl_certificates:is_trusted_cert(TrustedCert, CaCertFilePath)).

is_trusted_cert_returns_false_when_certificate_not_in_trust_store() ->
    CaCertFilePath = app_test_helpers:project_path(?MODULE, "test/unit/data/certificates/ca-certificates.crt"),

    UntrustedCertFilePath = app_test_helpers:project_path(?MODULE, "test/unit/data/certificates/untrusted.crt"),

    UntrustedCert = read_single_cert_from_file(UntrustedCertFilePath),

    ?assertNot(deliv_ssl_certificates:is_trusted_cert(UntrustedCert, CaCertFilePath)).

is_trusted_cert_returns_false_when_certificate_in_trust_store_but_is_expired() ->
    CaCertFilePath = app_test_helpers:project_path(?MODULE, "test/unit/data/certificates/ca-certificates.crt"),

    ExpiredCertFilePath = app_test_helpers:project_path(?MODULE, "test/unit/data/certificates/expired.crt"),

    ExpiredCert = read_single_cert_from_file(ExpiredCertFilePath),

    ?assertNot(deliv_ssl_certificates:is_trusted_cert(ExpiredCert, CaCertFilePath)).

is_trusted_cert_returns_false_when_trust_store_is_empty() ->
    CaCertFilePath = "fake/path/to/empty/file",
    TrustedCertFilePath = app_test_helpers:project_path(?MODULE, "test/unit/data/certificates/trusted.crt"),
    {ok, TrustedCertFileContent} = file:read_file(TrustedCertFilePath),

    hoax:mock(file, [
              ?expect(read_file,
                      ?withArgs([CaCertFilePath]),
                      ?andReturn({ok, <<>>}))]),

    [{'Certificate', TrustedCertDer, not_encrypted}] = public_key:pem_decode(TrustedCertFileContent),
    TrustedCert = public_key:pkix_decode_cert(TrustedCertDer, otp),

    ?assertNot(deliv_ssl_certificates:is_trusted_cert(TrustedCert, CaCertFilePath)).

is_trusted_cert_returns_false_when_trust_store_cannot_be_read() ->
    CaCertFilePath = "fake/path/to/empty/file",
    TrustedCertFilePath = app_test_helpers:project_path(?MODULE, "test/unit/data/certificates/trusted.crt"),
    {ok, TrustedCertFileContent} = file:read_file(TrustedCertFilePath),

    hoax:mock(file, [
              ?expect(read_file,
                      ?withArgs([CaCertFilePath]),
                      ?andReturn({error, reason}))]),

    [{'Certificate', TrustedCertDer, not_encrypted}] = public_key:pem_decode(TrustedCertFileContent),
    TrustedCert = public_key:pkix_decode_cert(TrustedCertDer, otp),

    ?assertNot(deliv_ssl_certificates:is_trusted_cert(TrustedCert, CaCertFilePath)).

read_single_cert_from_file(FilePath) ->
    {ok, FileContent} = file:read_file(FilePath),
    [{'Certificate', CertDer, not_encrypted}] = public_key:pem_decode(FileContent),
    public_key:pkix_decode_cert(CertDer, otp).

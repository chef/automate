%% @doc A module defining ssl verify functions for different certificate
%% validation schemes.
-module(deliv_ssl_certificates).

-include_lib("public_key/include/public_key.hrl").

-export([is_trusted_cert/2]).

%% @doc Checks if the passed certificate is trusted in the CA cert file given.
-spec is_trusted_cert(#'OTPCertificate'{}, iodata()) -> boolean().
is_trusted_cert(Cert, CaCertFilePath) ->
    case file:read_file(CaCertFilePath) of
        {ok, CaCertFileContent} ->
            CertEntries = public_key:pem_decode(CaCertFileContent),
            process_certificate_enteries(Cert, CertEntries);
        {error, _} -> false
    end.

-spec process_certificate_enteries(#'OTPCertificate'{}, [public_key:pem_entry()]) -> boolean().
process_certificate_enteries(_Cert, []) ->
    false;
process_certificate_enteries(Cert, [{'Certificate', CaCertDer, not_encrypted} | T]) ->
    case public_key:pkix_path_validation(Cert, [CaCertDer], []) of
        {ok, _CertificateInfo} -> true;
        _ -> process_certificate_enteries(Cert, T)
    end.

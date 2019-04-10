%% @doc A module defining ssl verify functions for different certificate
%% validation schemes.
-module(deliv_ssl_verify_functions).

-export([verify_selfsigned_cert/3]).

-include("deliv_types.hrl").
-include_lib("public_key/include/public_key.hrl").

-type ssl_verify_event() :: valid | valid_peer | {bad_cert, atom()} | {revoked, atom()} | {extension, #'Extension'{}}.

%% @doc Implementation verify_fun for ssl_options.
%%      Trusts selfsigned certificates when they are valid and in the provided
%%      trust store.
-spec verify_selfsigned_cert(#'OTPCertificate'{}, ssl_verify_event(), CertFilePath :: binary()) ->
        {valid, CertFilePath :: binary()} | {bad_cert, untrusted_selfsigned_peer}.
verify_selfsigned_cert(_, valid, CertFilePath) ->
    {valid, CertFilePath};
verify_selfsigned_cert(_, valid_peer, CertFilePath) ->
    {valid, CertFilePath};
verify_selfsigned_cert(Cert, {bad_cert, selfsigned_peer}, CertFilePath) ->
    case deliv_ssl_certificates:is_trusted_cert(Cert, CertFilePath) of
        true -> {valid, CertFilePath};
        false -> {bad_cert, untrusted_selfsigned_peer}
    end;
verify_selfsigned_cert(_, {bad_cert, _} = Reason, _) ->
    {fail, Reason};
verify_selfsigned_cert(_, {revoked, _} = Reason, _) ->
    {fail, Reason};
verify_selfsigned_cert(_, {extension, _}, CertFilePath) ->
    {unknown, CertFilePath}.

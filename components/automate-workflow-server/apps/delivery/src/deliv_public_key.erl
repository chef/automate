%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 80 -*-
%% ex: ts=4 sw=4 et

%% @doc Helper functions for dealing with public key data.
-module(deliv_public_key).

-export([
         parse/1
        ]).

-include_lib("chef_common/include/chef_common.hrl").

%% @doc Parse key data, provided as a binary, returning an
%% RSAPublicKey record or DSA key value. This function understands
%% OpenSSH style public keys (RSA and DSA) as well as RSA public keys
%% in PEM format encoded in PKCS1 and X509 SPKI formats.
%%
%% Returns `{error, bad_key}' if the key cannot be parsed.
-spec parse(binary()) -> rsa_public_key() | {error, bad_key}.
parse(BinKey = <<"-----BEGIN", _/binary>>) ->
    case extract_public_or_private_key(BinKey) of
        #'RSAPublicKey'{} = Key ->
            Key;
        _ ->
            {error, bad_key}
    end;
parse(BinSshKey) when is_binary(BinSshKey) ->
    try public_key:ssh_decode(BinSshKey, public_key) of
        [{PubKey, _List}] -> PubKey;
        [] -> {error, bad_key}
    catch
        error:_ ->
            {error, bad_key}
    end.

%% @doc Given PEM content as binary, return either an RSA public or
%% private key record (or error tuple). The PEM can contain an RSA
%% public key in PKCS1, SPKI (X509), or an X509 certificate wrapping
%% an SPKI formatted key. Note that private keys will not be extracted
%% from X509 certificate data.
-spec extract_public_or_private_key(binary()) -> #'RSAPublicKey'{}  |
                                                 #'RSAPrivateKey'{} |
                                                 {error, bad_key}.
extract_public_or_private_key(RawKey) ->
    try
        [Key] = public_key:pem_decode(RawKey),
        process_key(Key)
    catch
        _:_ ->
            {error, bad_key}
    end.

-spec process_key(public_key:pem_entry()) ->
                         rsa_public_key() |
                         rsa_private_key() |
                         {error, bad_key}.
process_key({'SubjectPublicKeyInfo', _, _} = PubEntry) ->
    public_key:pem_entry_decode(PubEntry);
process_key({'RSAPublicKey', Der, _}) ->
    public_key:der_decode('RSAPublicKey', Der);
process_key({'RSAPrivateKey', Der, _}) ->
        public_key:der_decode('RSAPrivateKey', Der);
process_key({'Certificate', _Der, _} = CertEntry) ->
    %% NOTE: assumes the certificate contains public key info and only extracts that.
    Cert = public_key:pem_entry_decode(CertEntry),
    TbsCert = Cert#'Certificate'.tbsCertificate,
    Spki = TbsCert#'TBSCertificate'.subjectPublicKeyInfo,
    {0, KeyDer} = Spki#'SubjectPublicKeyInfo'.subjectPublicKey,
    public_key:der_decode('RSAPublicKey', KeyDer).

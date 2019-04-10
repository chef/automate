-module(jobs_key).

-include("jobs_types.hrl").

-export([
         hydrate/1
        ]).

%% this is put into the openssl genrsa call as-is, so it's a string
-ifdef(TEST).
-define(RSA_KEY_LENGTH, "1024").
-else.
-define(RSA_KEY_LENGTH, "4096").
-endif.

-spec hydrate(jobs_runner()) -> jobs_runner() | {error, any()}.
hydrate(#runner{private_key = undefined} = Runner) ->
    hydrate(Runner, generate_private_key());
hydrate(#runner{public_key = undefined, private_key = PrivKey, hostname = Hostname} = Runner) ->
    PublicKey = get_public_key(PrivKey, Hostname),
    Runner#runner{public_key = PublicKey};
hydrate(#runner{} = Runner) ->
    Runner.

-spec hydrate(jobs_runner(), {error, any()} | binary()) -> jobs_runner() | {error, any()}.
hydrate(#runner{}, {error, _} = Error) -> Error;
hydrate(#runner{} = Runner, PrivateKey) ->
    hydrate(Runner#runner{private_key = PrivateKey}).

-spec generate_private_key() -> rsa_private_key() | {error, any()}.
generate_private_key() ->
    spawn_gen_priv_key(os:find_executable("openssl")).

-spec spawn_gen_priv_key(false | binary()) -> binary() | {error, no_executable_found | no_key_produced | timeout}.
spawn_gen_priv_key(false) ->
    chef_log:error("Could not generate RSA private key: openssl executable not found"),
    {error, no_executable_found};
spawn_gen_priv_key(Exec) ->
    case chef_utils:run_cmd([Exec, "genrsa", ?RSA_KEY_LENGTH, "2>/dev/null"]) of
        {0, Output} -> try_deserialize(Output);
        {error, timeout} = Error ->
            chef_log:error("Could not generate RSA private key: exec=~s timeout", [Exec]),
            Error;
        {Nonzero, Output} ->
            chef_log:error("Could not generate RSA private key: exec=~s exitcode=~p, output=~s", [Exec, Nonzero, Output]),
            {error, no_key_produced}
    end.

-spec try_deserialize(binary()) -> binary() | {error, no_key_produced}.
try_deserialize(Output) ->
    case deserialize_private_key(Output) of
        error -> {error, no_key_produced};
        _ -> Output
    end.

-spec deserialize_private_key(binary()) -> rsa_private_key() | error.
deserialize_private_key(PEM) ->
    case public_key:pem_decode(PEM) of
        [{'PrivateKeyInfo', _, not_encrypted} = Entry]  ->
            KeyInfo = public_key:pem_entry_decode(Entry),
            KeyAlgorithmInfo = KeyInfo#'PrivateKeyInfo'.privateKeyAlgorithm,
            case KeyAlgorithmInfo of
                #'PrivateKeyInfo_privateKeyAlgorithm'{algorithm=?'rsaEncryption'} ->
                    PrivateKey = KeyInfo#'PrivateKeyInfo'.privateKey,
                    public_key:der_decode('RSAPrivateKey', PrivateKey);
                _ ->
                    {error, bad_key}
            end;
        [PemEntry] -> public_key:pem_entry_decode(PemEntry);
        _ -> error
    end.

-spec get_public_key(rsa_private_key() | binary(), binary()) -> binary().
get_public_key(PrivateKey, Hostname) when is_binary(PrivateKey) ->
    get_public_key(deserialize_private_key(PrivateKey), Hostname);
get_public_key(#'RSAPrivateKey'{modulus = Mod,
                                publicExponent = PubExp}, Hostname) ->
    PubKey = #'RSAPublicKey'{modulus = Mod, publicExponent = PubExp},
    public_key:ssh_encode([{PubKey, [{comment, <<"job_runner@", Hostname/binary>>}]}], openssh_public_key).

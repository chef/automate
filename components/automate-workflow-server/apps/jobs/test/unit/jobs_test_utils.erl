-module(jobs_test_utils).

-include("jobs_types.hrl").

-compile(export_all).

test_private_key() ->
    test_private_key(record).

test_private_key_binary() ->
    test_private_key(binary).

test_private_key(record) ->
    #'RSAPrivateKey'{version = 'two-prime',
                     modulus = 9,
                     publicExponent = 65537,
                     privateExponent = 7,
                     prime1 = 2,
                     prime2 = 3,
                     exponent1 = 4,
                     exponent2 = 9,
                     coefficient = 13,
                     otherPrimeInfos = asn1_NOVALUE};
test_private_key(binary) ->
    <<"-----BEGIN RSA PRIVATE KEY-----\nMB0CAQACAQkCAwEAAQIBBwIBAgIBAwIBBAIBCQIBDQ==\n-----END RSA PRIVATE KEY-----\n\n">>.

test_public_key() ->
    test_public_key(record).

test_public_key(record) ->
    #'RSAPublicKey'{modulus = 9,
                    publicExponent = 65537}.

test_public_key_binary() ->
    <<"ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAAAAQk= job_runner@one.runner\n">>.

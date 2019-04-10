-module(deliv_ssh_git_tests).

-include_lib("hoax/include/hoax.hrl").

start_calls_ssh_daemon_with_proper_arguments_test() ->
    hoax:test(fun() ->
        HostAddress = "0.0.0.0",
        {ok, ParsedHostAddress} = inet:parse_address(HostAddress),
        Port = 9898,
        KeysPath = "/git/server_keys",
        %% as of OTP18
        DefaultAlgs = [{kex,['ecdh-sha2-nistp256','ecdh-sha2-nistp384',
                             'diffie-hellman-group14-sha1',
                             'diffie-hellman-group-exchange-sha256',
                             'diffie-hellman-group-exchange-sha1','ecdh-sha2-nistp521',
                             'diffie-hellman-group1-sha1']},
                       {public_key,['ecdsa-sha2-nistp256','ecdsa-sha2-nistp384',
                                    'ecdsa-sha2-nistp521','ssh-rsa','ssh-dss']},
                       {cipher,[{client2server,['aes256-ctr','aes192-ctr',
                                                'aes128-ctr','aes128-cbc','aes128-gcm@openssh.com',
                                                'aes256-gcm@openssh.com','3des-cbc']},
                                {server2client,['aes256-ctr','aes192-ctr','aes128-ctr',
                                                'aes128-cbc','aes128-gcm@openssh.com',
                                                'aes256-gcm@openssh.com','3des-cbc']}]},
                       {mac,[{client2server,['hmac-sha2-256','hmac-sha2-512',
                                             'hmac-sha1']},
                             {server2client,['hmac-sha2-256','hmac-sha2-512',
                                             'hmac-sha1']}]},
                       {compression,[{client2server,[none,'zlib@openssh.com',zlib]},
                                     {server2client,[none,'zlib@openssh.com',zlib]}]}],
        StrongAlgs = [{kex,['diffie-hellman-group14-sha1',
                            'diffie-hellman-group-exchange-sha256']},
                      {public_key,['ecdsa-sha2-nistp256','ecdsa-sha2-nistp384',
                                   'ecdsa-sha2-nistp521','ssh-rsa']},
                      {cipher,[{client2server,['aes256-ctr','aes192-ctr',
                                               'aes128-ctr','aes128-cbc','aes128-gcm@openssh.com',
                                               'aes256-gcm@openssh.com','3des-cbc']},
                               {server2client,['aes256-ctr','aes192-ctr','aes128-ctr',
                                               'aes128-gcm@openssh.com',
                                               'aes256-gcm@openssh.com']}]},
                      {mac,[{client2server,['hmac-sha2-256','hmac-sha2-512',
                                            'hmac-sha1']},
                            {server2client,['hmac-sha2-256','hmac-sha2-512',
                                            'hmac-sha1']}]},
                      {compression,[{client2server,[none,'zlib@openssh.com',zlib]},
                                    {server2client,[none,'zlib@openssh.com',zlib]}]}],
        Opts = [{system_dir, KeysPath},
                {ssh_cli, {deliv_ssh_git, []}},
                {auth_methods, "publickey"},
                {negotiation_timeout, 5000},
                {key_cb, deliv_ssh_key_cb},
                {preferred_algorithms, StrongAlgs}],
        hoax:expect(receive
                        delivery_app:start_app_with_deps(ssh, permanent) -> ok;

                        delivery_app:get_env(deliv_ssh_git_server_keys_path) -> KeysPath;
                        delivery_app:get_env(deliv_ssh_git_host_address, HostAddress) -> HostAddress;
                        delivery_app:get_env(deliv_ssh_git_port) -> Port;
                        ssh:default_algorithms() -> DefaultAlgs;
                        ssh:daemon(ParsedHostAddress, Port, Opts) -> ssh_daemon_return
                    end),
        ?assertEqual(ssh_daemon_return, deliv_ssh_git:start()),
        ?verifyAll
    end).

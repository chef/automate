-module(jobs_ssh_command_tests).

-include("jobs_types.hrl").
-include_lib("hoax/include/hoax.hrl").

-compile(export_all).

jobs_runner_test_() ->
    [
     hoax:fixture(?MODULE, "generate_ssh_command_")
    ].

generate_ssh_command_generate_command() ->
    Command = <<"ls -al">>,
    Hostname = <<"hostname">>,
    PrivKey = jobs_test_utils:test_private_key_binary(),
    SshCommand = generate_ssh_command(Hostname, Command),
    hoax:expect(receive
                    chef_utils:iodata_to_str(SshCommand) -> ssh_command_str
                end),

    Actual = jobs_ssh_command:generate_ssh_command(#runner{hostname = Hostname, private_key = PrivKey}, Command),

    ?assertEqual(ssh_command_str, Actual),
    ?verifyAll.

generate_ssh_command(Hostname, Command) ->
    [generate_ssh_helper_path(), " '", jobs_test_utils:test_private_key(binary),
     "' ", Hostname, " '", Command, "'"].

generate_ssh_helper_path() ->
    [code:priv_dir(jobs), "/ssh_helper.sh"].

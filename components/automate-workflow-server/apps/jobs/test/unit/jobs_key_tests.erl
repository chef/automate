-module(jobs_key_tests).

-include("jobs_types.hrl").
-include_lib("hoax/include/hoax.hrl").

-compile(export_all).

jobs_key_test_() ->
    [
     eunit_sugar:parameterized_fixture(?MODULE, "bad_path_hydrate_", break_path, repair_path),
     hoax:fixture(?MODULE, "hydrate_")
    ].

%% there's no way to mock os:*, so we have to mess with the env
break_path() ->
    OldPath = os:getenv("PATH"),
    os:putenv("PATH", "/nope"),
    OldPath.

repair_path(OldPath) ->
    os:putenv("PATH", OldPath).

bad_path_hydrate_when_openssl_cannot_be_found_returns_error(_) ->
    Actual = jobs_key:hydrate(#runner{}),
    ?assertMatch({error, no_executable_found}, Actual).

hydrate_populates_runner_with_public_key_when_private_key_exists() ->
    RunnerWithHostname = #runner{hostname = <<"one.runner">>,
                                 private_key = jobs_test_utils:test_private_key_binary()},

    Result = jobs_key:hydrate(RunnerWithHostname),

    Expected = #runner{hostname = <<"one.runner">>,
                       private_key = jobs_test_utils:test_private_key_binary(),
                       public_key = jobs_test_utils:test_public_key_binary()
                      },
    ?assertEqual(Expected, Result).

hydrate_returns_runner_when_fully_hydrated() ->
    RunnerWithHostname = #runner{hostname = <<"one.runner">>,
                                 private_key = jobs_test_utils:test_private_key_binary(),
                                 public_key = jobs_test_utils:test_public_key_binary()},

    ?assertEqual(RunnerWithHostname, jobs_key:hydrate(RunnerWithHostname)).

hydrate_generates_a_private_key() ->
    RunnerWithHostname = #runner{hostname = <<"one.runner">>},

    #runner{private_key = PrivateKey} = jobs_key:hydrate(RunnerWithHostname),

    [PemEntry] = public_key:pem_decode(PrivateKey),

    ?assertMatch(#'RSAPrivateKey'{}, public_key:pem_entry_decode(PemEntry)).

hydrate_populates_runner_with_private_and_public_keys() ->
    RunnerWithHostname = #runner{hostname = <<"one.runner">>},

    hoax:expect(receive
                    chef_utils:run_cmd(?any) -> {0, jobs_test_utils:test_private_key_binary()}
                end),

    Result = jobs_key:hydrate(RunnerWithHostname),
    Expected = #runner{hostname = <<"one.runner">>,
                       private_key = jobs_test_utils:test_private_key_binary(),
                       public_key = jobs_test_utils:test_public_key_binary()
                      },
    ?assertEqual(Expected, Result),
    ?verifyAll.

hydrate_populates_runner_with_private_and_public_keys_works_with_fips() ->
    RunnerWithHostname = #runner{hostname = <<"one.runner">>},
    {ok, PrivateKey} = file:read_file(app_test_helpers:project_path(?MODULE, "test/unit/data/fips-private-key")),

    hoax:expect(receive
                    chef_utils:run_cmd(?any) -> {0, PrivateKey}
                end),

    Result = jobs_key:hydrate(RunnerWithHostname),
    Expected = #runner{hostname = <<"one.runner">>,
                       private_key = PrivateKey,
                       public_key = <<"ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCU73q3/gM7HQ0lC7mLwfUs0vJMXoG9uD1A4KLR/FXq3FGWAjhuezD+3Pi059bHW/c1Crl3ByJOZEe9RX6jzHY4OfsuAFHx6/35uh+LX1NASP0hk904CGlcGMrHdD7LvIo4kHZxPYY6gSOxfxRcQ9DUwWqzfg5Nns4003SNQRKVD2uB9E5Tn5qtGZ7q5lQEC19BoqQek/egAjqZly4XkrNWw9XHnaI0juLRt3NTpjp3+xWzIWG2O9EaFC/mFkskSbjlexVlb5hvFrWNz7aKOBW8C95vGhfg1yi8oVWeK/3B3IeNeYHFoBi91gAYpZA/ChTYA5blg66ymRlRnx0Belzr job_runner@one.runner\n">>
                      },
    ?assertEqual(Expected, Result),
    ?verifyAll.

hydrate_when_command_times_out_logs_and_returns_error() ->
    %% note: the path of openssl differs in local testing, so this is left a
    %% wildcard
    hoax:expect(receive
                    chef_utils:run_cmd(?any) -> {error, timeout};
                    chef_log:error(?any, ?any) -> ok
                end),
    Actual = jobs_key:hydrate(#runner{}),
    ?assertEqual({error, timeout}, Actual),
    ?verifyAll.

hydrate_when_command_returns_nonzero_exitcode_logs_and_returns_error() ->
    hoax:expect(receive
                    chef_utils:run_cmd(?any) -> {1, <<"probably an error message">>};
                    chef_log:error(?any, ?any) -> ok
                end),
    Actual = jobs_key:hydrate(#runner{}),
    ?assertEqual({error, no_key_produced}, Actual),
    ?verifyAll.

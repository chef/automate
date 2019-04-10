-module(jobs_runner_db_integration_tests).

-include("jobs_types.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile([export_all]).

jobs_runner_test_() ->
    [
     eunit_sugar:parameterized_fixture(?MODULE, "delete_", setup, teardown),
     eunit_sugar:parameterized_fixture(?MODULE, "fetch_by_name_", setup, teardown),
     eunit_sugar:parameterized_fixture(?MODULE, "fetch_all_", setup, teardown),
     eunit_sugar:parameterized_fixture(?MODULE, "insert_", setup, teardown)
    ].

setup() ->
    error_logger:tty(false),
    eu_database:setup(),
    eu_data:with_enterprise(<<"ent_name">>, fun(_) ->
                                                    <<"ent_name">>
                                            end).

teardown(_) ->
    eu_database:teardown(),
    error_logger:tty(true).

delete_when_runner_exists_deletes_runner() ->
    Runner = #runner{hostname = <<"one.runner">>,
                     os = <<"linux">>,
                     platform = <<"ubuntu">>,
                     platform_version = <<"14.04">>,
                     platform_family = <<"debian">>,
                     private_key = jobs_test_utils:test_private_key_binary(),
                     pid = some_pid},
    [NewRunner] = jobs_runner_db:insert(Runner),
    ?assertEqual({ok, 1}, jobs_runner_db:delete(NewRunner)),
    ?assertEqual([], jobs_runner_db:fetch_all()).

fetch_by_name_when_runner_does_not_exist_returns_empty_list(EntName) ->
    Hostname = <<"one.runner">>,
    OtherHostname = <<"two.runner">>,
    Runner = #runner{hostname = Hostname,
                     os = <<"linux">>,
                     platform = <<"ubuntu">>,
                     platform_version = <<"14.04">>,
                     platform_family = <<"debian">>,
                     private_key = jobs_test_utils:test_private_key_binary(),
                     pid = some_pid},
    jobs_runner_db:insert(EntName, Runner),
    ?assertEqual([], jobs_runner_db:fetch_by_name(EntName, OtherHostname, some_pid)).

fetch_by_name_when_runner_exists_successfully_returns_runner(EntName) ->
    Hostname = <<"one.runner">>,
    Runner = #runner{hostname = Hostname,
                     os = <<"linux">>,
                     platform = <<"ubuntu">>,
                     platform_version = <<"14.04">>,
                     platform_family = <<"debian">>,
                     private_key = jobs_test_utils:test_private_key_binary(),
                     pid = some_pid},
    [NewRunner] = jobs_runner_db:insert(EntName, Runner),
    ?assertEqual([NewRunner], jobs_runner_db:fetch_by_name(EntName, Hostname, some_pid)),
    ?assertEqual(jobs_test_utils:test_public_key_binary(), NewRunner#runner.public_key).

fetch_all_when_no_runner_exists_successfully_returns_empty_list(_) ->
    Result = jobs_runner_db:fetch_all(),
    ?assertEqual([], Result).

fetch_all_when_multiple_runners_exists_successfully_returns_list_of_deserialized_runners(EntName) ->
    Hostname1 = <<"one.runner">>,
    Hostname2 = <<"two.runner">>,
    Pid = undefined,
    Key = jobs_test_utils:test_private_key_binary(),
    PubKey1 = jobs_test_utils:test_public_key_binary(),
    PubKey2 = <<"ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAAAAQk= job_runner@two.runner\n">>,

    Os = <<"linux">>,
    Platform = <<"ubuntu">>,
    PlatformVersion = <<"14.04">>,
    PlatformFamily =  <<"debian">>,

    Runner1 = #runner{
                 hostname = Hostname1,
                 os = Os,
                 platform = Platform,
                 platform_version = PlatformVersion,
                 platform_family = PlatformFamily,
                 private_key = Key,
                 pid = Pid
                },

    Runner2 = #runner{
                 hostname = Hostname2,
                 os = Os,
                 platform = Platform,
                 platform_version = PlatformVersion,
                 platform_family = PlatformFamily,
                 private_key = Key,
                 pid = Pid
                },

    jobs_runner_db:insert(EntName, Runner1),
    jobs_runner_db:insert(EntName, Runner2),
    Result = jobs_runner_db:fetch_all(),
    ?assertMatch([#runner{enterprise_name = EntName, hostname = Hostname1,
                          private_key = Key, public_key = PubKey1, pid = Pid,
                          os = Os, platform = Platform,
                          platform_version = PlatformVersion, platform_family = PlatformFamily},
                  #runner{enterprise_name = EntName, hostname = Hostname2,
                          private_key = Key, public_key = PubKey2, pid = Pid,
                          os = Os, platform = Platform,
                          platform_version = PlatformVersion, platform_family = PlatformFamily}],
                 lists:keysort(5, Result)).

insert_when_no_runner_with_this_hostname_exists_adds_a_runner(EntName) ->
    Runner = #runner{hostname = <<"one.runner">>,
                     private_key = jobs_test_utils:test_private_key_binary(),
                     pid = undefined,
                     os = <<"linux">>,
                     platform = <<"ubuntu">>,
                     platform_version = <<"14.04">>,
                     platform_family = <<"debian">>
                    },
    NewRunners = jobs_runner_db:insert(EntName, Runner),

    ?assertEqual(NewRunners, jobs_runner_db:fetch_all()).

insert_when_insert_fails_no_records_are_saved(_) ->
    Runner = #runner{hostname = <<"one.runner">>,
                     private_key = jobs_test_utils:test_private_key_binary(),
                     pid = some_pid,
                     os = <<"linux">>,
                     platform = <<"ubuntu">>,
                     platform_version = <<"14.04">>,
                     platform_family = <<"debian">>
                    },

    {error, _} = jobs_runner_db:insert(<<"no-ent">>, Runner),

    ?assertEqual([], jobs_runner_db:fetch_all()).

insert_when_provided_error_tuple_returns_error_tuple(EntName) ->
    ?assertEqual({error, r}, jobs_runner_db:insert(EntName, {error, r})).

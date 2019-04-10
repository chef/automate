-module(jobs_runner_registry_tests).

-include("jobs_types.hrl").
-include_lib("hoax/include/hoax.hrl").

-compile(export_all).

jobs_runner_registry_test_() ->
    [
     eunit_sugar:fixture(?MODULE, "init_"),
     hoax:fixture(?MODULE, "handle_info_timeout_"),
     hoax:fixture(?MODULE, "handle_info_publish_"),
     hoax:fixture(?MODULE, "handle_call_register_"),
     hoax:fixture(?MODULE, "handle_call_delete_"),
     hoax:fixture(?MODULE, "handle_call_schedule_job_run_"),
     hoax:fixture(?MODULE, "handle_call_health_")
    ].

init_should_set_initial_state_test() ->
    InitialState = jobs_runner_registry:init([]),
    ?assertEqual({ok, [], 0}, InitialState).

handle_info_timeout_should_load_registry_from_storage() ->
    NewState = [{{some_ent, hostname2}, pid2}, {{some_ent, hostname1}, pid1}],
    Runner1 = #runner{enterprise_name = some_ent,
                      hostname = hostname1,
                      os = os,
                      platform = platform,
                      platform_version = platform_version,
                      platform_family = platform_family},

    Runner2 = #runner{enterprise_name = some_ent,
                      hostname = hostname2,
                      os = os,
                      platform = platform,
                      platform_version = platform_version,
                      platform_family = platform_family},

    hoax:expect(receive
                    jobs_runner_db:fetch_all() -> [Runner1, Runner2];
                    jobs_runners_sup:create(some_ent, hostname1, os, platform_family, platform, platform_version) -> {ok, pid1};
                    jobs_runners_sup:create(some_ent, hostname2, os, platform_family, platform, platform_version) -> {ok, pid2};
                    jobs_runner:fetch_state(pid1) -> Runner1;
                    jobs_runner:fetch_state(pid2) -> Runner2
                end),

    Actual = jobs_runner_registry:handle_info(timeout, []),

    ?assertEqual({noreply, NewState}, Actual),
    ?verifyAll.

handle_call_register_when_no_existing_runner_should_invoke_sup_then_store_return_result() ->
    Runner = #runner{enterprise_name = ent,
                     hostname = hostname,
                     os = os,
                     platform = platform,
                     platform_version = platform_version,
                     platform_family = platform_family},
    hoax:expect(receive
                    jobs_runners_sup:create(ent, hostname, os, platform_family, platform, platform_version) -> {ok, some_pid};
                    jobs_runner:fetch_state(some_pid) -> Runner
                end),

    {reply, Reply, NewState} = jobs_runner_registry:handle_call({create, ent, hostname, os, platform_family,
                                                                 platform, platform_version}, from, []),

    ?assertEqual([{{ent,hostname}, some_pid}], NewState),
    ?assertEqual({ok, Runner}, Reply),
    ?verifyAll.

handle_call_register_when_other_existing_runner_should_invoke_sup_then_store_return_result() ->
    Runner = #runner{enterprise_name = ent,
                     hostname = hostname,
                     os = os,
                     platform = platform,
                     platform_version = platform_version,
                     platform_family = platform_family},
    hoax:expect(receive
                    jobs_runners_sup:create(ent, hostname, os, platform_family, platform, platform_version) -> {ok, some_pid};
                    jobs_runner:fetch_state(some_pid) -> Runner
                end),

    {reply, Reply, NewState} = jobs_runner_registry:handle_call({create, ent, hostname, os, platform_family,
                                                                 platform, platform_version}, from,
                                                                [{{ent, other_hostname}, other_pid}]),

    ?assertEqual([{{ent,hostname}, some_pid}, {{ent, other_hostname}, other_pid}], NewState),
    ?assertEqual({ok, Runner}, Reply),
    ?verifyAll.

handle_call_register_when_existing_runner_returns_existing_runner() ->
    Runner = #runner{enterprise_name = ent,
                     hostname = hostname,
                     os = os,
                     platform = platform,
                     platform_version = platform_version,
                     platform_family = platform_family},

    hoax:expect(receive
                    jobs_runner:fetch_state(some_pid) -> Runner
                end),

    {reply, Reply, NewState} = jobs_runner_registry:handle_call({create, ent, hostname, os, platform_family,
                                                                 platform, platform_version}, from,
                                                                [{{ent, hostname}, some_pid}]),

    ?assertEqual([{{ent,hostname}, some_pid}], NewState),
    ?assertEqual({ok, Runner}, Reply),
    ?verifyAll.

handle_call_register_get_public_key_calls_create_returns_key() ->
    Runner = #runner{enterprise_name = ent,
                     hostname = <<"one.runner">>,
                     os = os,
                     platform = platform,
                     platform_version = platform_version,
                     platform_family = platform_family,
                     public_key = jobs_test_utils:test_public_key_binary()},

    hoax:expect(receive
                    jobs_runner:fetch_state(some_pid) -> Runner
                end),

    {reply, Reply, NewState} = jobs_runner_registry:handle_call({create_get_public_key, ent, hostname, os, platform_family,
                                                                 platform, platform_version}, from,
                                                                [{{ent, hostname}, some_pid}]),

    ?assertEqual([{{ent,hostname}, some_pid}], NewState),
    ?assertEqual({ok, jobs_test_utils:test_public_key_binary()}, Reply),
    ?verifyAll.

handle_call_delete_when_single_existing_runner_returns_ok() ->
    State = [{{some_ent, hostname}, some_pid}],
    Runner = #runner{enterprise_name = some_ent, hostname = hostname, pid = some_pid},

    hoax:expect(receive
                    jobs_runner:delete(some_pid) -> ok;
                    jobs_runners_sup:delete(some_pid) -> ok
                end),

    {reply, Reply, NewState} = jobs_runner_registry:handle_call({delete, Runner}, from, State),

    ?assertEqual([], NewState),
    ?assertEqual(ok, Reply),
    ?verifyAll.

handle_call_health_when_runner_is_registered_calls_health_in_pid() ->
    Pid = erlang:list_to_pid("<0.1.2>"),
    Runner = #runner{pid = Pid},
    NewRunnerState = new_runner,

    hoax:expect(receive
                    jobs_runner:health(Pid) -> NewRunnerState
                end),

    ?assertEqual({reply, NewRunnerState, state}, jobs_runner_registry:handle_call({health, Runner}, from, state)),
    ?verifyAll.

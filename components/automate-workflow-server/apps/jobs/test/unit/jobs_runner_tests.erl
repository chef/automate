-module(jobs_runner_tests).

-include("jobs_types.hrl").
-include_lib("hoax/include/hoax.hrl").

-compile(export_all).

jobs_runner_test_() ->
    [
     hoax:fixture(?MODULE, "init_"),
     hoax:fixture(?MODULE, "handle_call_"),
     hoax:fixture(?MODULE, "handle_info_"),
     eunit_sugar:fixture(?MODULE, "format_status_"),
     hoax:fixture(?MODULE, "subscribe_runner_events_"),
     hoax:fixture(?MODULE, "terminate_")
    ].

init_when_no_runner_with_that_hostname_exists_creates_runner() ->
    Hostname = <<"one.runner">>,
    RunnerPassedToHydrate = #runner{hostname = Hostname,
                                    os = os,
                                    platform = platform,
                                    platform_version = platform_version,
                                    platform_family = platform_family,
                                    pid = self()},

    RunnerWithKey = #runner{hostname = Hostname,
                            private_key = private_key,
                            os = os,
                            platform = platform,
                            platform_version = platform_version,
                            platform_family = platform_family,
                            pid = self()},

    PersistedRunner = RunnerWithKey#runner{id = id,
                                           os = os,
                                           platform = platform,
                                           platform_version = platform_version,
                                           platform_family = platform_family,
                                           enterprise_name = some_ent,
                                           public_key = public_key},

    hoax:expect(receive
                    jobs_runner_db:fetch_by_name(some_ent, Hostname, self()) -> [];
                    jobs_key:hydrate(RunnerPassedToHydrate) -> RunnerWithKey;
                    jobs_runner_db:insert(some_ent, RunnerWithKey) -> [PersistedRunner];
                    chef_utils:send_after(1000, self(), request_job) -> unused;
                    deliv_event:publish(runner_state_changed, PersistedRunner#runner{private_key = undefined}) -> unused
                end),
    Actual = jobs_runner:init([some_ent, Hostname, os, platform_family, platform, platform_version]),
    ?assertMatch({ok, PersistedRunner}, Actual),
    ?verifyAll.

init_when_runner_with_that_hostname_exists_returns_that_runner() ->
    Hostname = <<"one.runner">>,
    FetchedRunner = #runner{id = id,
                            os = os,
                            platform = platform,
                            platform_version = platform_version,
                            platform_family = platform_family,
                            enterprise_name = some_ent,
                            hostname = Hostname,
                            private_key = private_key,
                            public_key = public_key,
                            pid = self()},
    hoax:expect(receive
                    jobs_runner_db:fetch_by_name(ent_name, Hostname, self()) -> [FetchedRunner];
                    chef_utils:send_after(1000, self(), request_job) -> unused;
                    deliv_event:publish(runner_state_changed, FetchedRunner#runner{private_key = undefined}) -> unused
                end),
    Actual = jobs_runner:init([ent_name, Hostname, os, platform_family, platform, platform_version]),
    ?assertMatch({ok, FetchedRunner}, Actual),
    ?verifyAll.

handle_call_health_does_not_spawn_health_check_if_health_check_is_in_progress() ->
    Runner = #runner{job = #health_job{},
                     private_key = private_key},
    ReturnedRunner = Runner#runner{private_key = undefined},

    Actual = jobs_runner:handle_call(health, from, Runner),

    ?assertEqual({reply, ReturnedRunner, Runner}, Actual),
    ?verifyAll.

handle_call_health_does_not_spawn_health_check_if_job_is_in_progress() ->
    Runner = #runner{job = #job{deliv_ssh_job_pid = deliv_pid}},
    NewRunner = Runner#runner{health_output = <<"Health check blocked on running job.">>,
                              health_status = undefined},
    ReturnedRunner = NewRunner#runner{private_key = undefined},

    Actual = jobs_runner:handle_call(health, from, Runner),

    ?assertEqual({reply, ReturnedRunner, Runner}, Actual),
    ?verifyAll.

handle_call_health_spawns_health_check_and_updates_state() ->
    Runner = #runner{health_status = ok,
                     health_output = <<"Success!">>,
                     private_key = private_key},
    NewRunner = Runner#runner{health_status = pending,
                              health_output = undefined,
                              job = #health_job{}},
    ReturnedRunner = NewRunner#runner{private_key = undefined},
    hoax:expect(receive
                    jobs_command:start(Runner, <<"ls -al 2>&1 > /dev/null && echo 'Success!'">>) -> pid;
                    deliv_event:publish(runner_state_changed, ReturnedRunner) -> unused
                end),

    Actual = jobs_runner:handle_call(health, from, Runner),

    ?assertEqual({reply, ReturnedRunner, NewRunner}, Actual),
    ?verifyAll.

handle_call_fetch_state_returns_state_as_runner_record_with_private_key_redacted() ->
    Runner = #runner{id = id, enterprise_name = some_ent, hostname = <<"hostname.0">>, private_key = private_key},

    ReturnedRunner = Runner#runner{private_key = undefined},
    Actual = jobs_runner:handle_call(fetch_state, from, Runner),
    ?assertEqual({reply, ReturnedRunner, Runner}, Actual),
    ?verifyAll.

handle_call_fetch_ent_hostname_returns_ent_hostname() ->
  Runner = #runner{enterprise_name = some_ent, hostname = <<"hostname.0">>},

  Actual = jobs_runner:handle_call(fetch_ent_hostname, from, Runner),
  ?assertEqual({reply, {some_ent, <<"hostname.0">>}, Runner}, Actual),
  ?verifyAll.

handle_call_delete_returns_ok() ->
    Runner = #runner{id = id, enterprise_name = some_ent, hostname = <<"hostname.0">>, private_key = private_key},
    hoax:expect(receive
                    jobs_runner_db:delete(Runner) -> {ok, row_int}
                end),

    Actual = jobs_runner:handle_call(delete, from, Runner),
    ?assertEqual({reply, ok, Runner}, Actual),
    ?verifyAll.

handle_call_delete_when_db_delete_fails_then_returns_error_does_not_stop_process() ->
    Runner = #runner{id = id, enterprise_name = some_ent, hostname = <<"hostname.0">>, private_key = private_key},
    hoax:expect(receive
                    jobs_runner_db:delete(Runner) -> {error, msg}
                end),

    Actual = jobs_runner:handle_call(delete, from, Runner),
    ?assertEqual({reply, {error, msg}, Runner}, Actual),
    ?verifyAll.

handle_call_cancel_job_sends_cancel_message_to_jobs_command() ->
    Runner = #runner{job = #job{jobs_command_pid = pid}},
    hoax:expect(receive
                    jobs_command:cancel(pid) -> ok
                end),

    Actual = jobs_runner:handle_call(cancel_job, from, Runner),

    ?assertEqual({reply, ok, Runner}, Actual),
    ?verifyAll.

handle_info_when_command_exits_normally() ->
    Actual = jobs_runner:handle_info({'EXIT', from, normal}, runner),

    ?assertEqual({noreply, runner}, Actual),
    ?verifyAll.

handle_info_when_health_check_process_dies_unexpectedly() ->
    Msg = <<"The erlang process running the health check died unexpectedly.">>,
    Runner = #runner{job = #health_job{},
                     health_status = pending},
    NewRunner = Runner#runner{job = undefined,
                              health_output = Msg,
                              health_status = error},
    hoax:expect(receive
                    chef_log:error("jobs_runner health check caught exit with reason: ~p", [not_normal]) -> ignored;
                    deliv_event:publish(runner_state_changed, NewRunner) -> unused
                end),

    Actual = jobs_runner:handle_info({'EXIT', from, not_normal}, Runner),

    ?assertEqual({noreply, NewRunner}, Actual),
    ?verifyAll.

handle_info_when_command_is_canceled_dequeues_job_informs_deliv_ssh_job_marks_undefined() ->
    Runner = #runner{job = #job{id = job_id, deliv_ssh_job_pid = deliv_pid}},
    NewRunner = Runner#runner{job = undefined},
    hoax:expect(receive
                    jobs_queue:remove_job(job_id) -> ignored;
                    deliv_ssh_job:job_canceled(deliv_pid) -> ignored;
                    deliv_event:publish(runner_state_changed, NewRunner) -> unused
                end),

    Actual = jobs_runner:handle_info({'EXIT', from, {shutdown, {error, canceled}}}, Runner),

    ?assertEqual({noreply, NewRunner}, Actual),
    ?verifyAll.

handle_info_when_command_fails_dequeues_job_informs_deliv_ssh_job_marks_undefined() ->
    Hostname = <<"some_host">>,
    Runner = #runner{hostname = Hostname,
                     job = #job{id = job_id, deliv_ssh_job_pid = deliv_pid}},
    NewRunner = Runner#runner{job = undefined},
    hoax:expect(receive
                    jobs_queue:remove_job(job_id) -> ignored;
                    deliv_ssh_job:job_dispatch_failed(deliv_pid, Hostname, <<"\nExit code: 1\n\nDetails:\nssh_failure\n">>) -> ignored;
                    deliv_event:publish(runner_state_changed, NewRunner) -> unused
                end),

    Actual = jobs_runner:handle_info({'EXIT', from, {shutdown, {1, <<"ssh_failure">>}}}, Runner),

    ?assertEqual({noreply, NewRunner}, Actual),
    ?verifyAll.

handle_info_when_command_exits_unexpectedly_logs_message() ->
    hoax:expect(receive
                    chef_log:error("Caught unexpected jobs_command EXIT signal from ~p with reason ~p", [from, unknown_error]) -> ignored
                end),

    Actual = jobs_runner:handle_info({'EXIT', from, unknown_error}, runner),

    ?assertEqual({noreply, runner}, Actual),
    ?verifyAll.

handle_info_when_exits_zero_job_results_removes_job_from_queue_and_updates_state() ->
    Runner = #runner{job = #job{id = job_id, deliv_ssh_job_pid = deliv_pid}},
    NewRunner = Runner#runner{job = undefined},
    hoax:expect(receive
                    jobs_queue:remove_job(job_id) -> ignored;
                    deliv_ssh_job:job_finished(deliv_pid) -> ignored;
                    deliv_event:publish(runner_state_changed, NewRunner) -> unused
                end),

    Actual = jobs_runner:handle_info({'EXIT', from, {shutdown, {0, output}}}, Runner),
    ?assertEqual({noreply, NewRunner}, Actual),
    ?verifyAll.

handle_info_health_check_finished_updates_state_with_ok() ->
    Runner = #runner{job = #health_job{}},
    NewRunner = Runner#runner{job = undefined,
                              health_output = output,
                              health_status = ok},
    hoax:expect(receive
                    deliv_event:publish(runner_state_changed, NewRunner) -> unused
                end),

    Actual = jobs_runner:handle_info({'EXIT', from, {shutdown, {0, output}}}, Runner),
    ?assertEqual({noreply, NewRunner}, Actual).

handle_info_health_check_finished_updates_state_with_error() ->
    Runner = #runner{job = #health_job{}},
    NewRunner = Runner#runner{job = undefined,
                              health_output = output,
                              health_status = error},
    hoax:expect(receive
                    deliv_event:publish(runner_state_changed, NewRunner) -> unused
                end),

    Actual = jobs_runner:handle_info({'EXIT', from, {shutdown, {1, output}}}, Runner),
    ?assertEqual({noreply, NewRunner}, Actual).

handle_info_request_job_tuple_when_pending_job_executes_job() ->
    Hostname = <<"some_host">>,
    [Os, PlatformFamily, Platform, PlatformVersion] = [<<"linux">>, <<"debian">>, <<"ubuntu">>, <<"15.04">>],
    Command = <<"delivery-cmd something">>,
    Runner = #runner{hostname = Hostname,
                     private_key = private_key,
                     job = undefined,
                     os = Os,
                     platform_family = PlatformFamily,
                     platform = Platform,
                     platform_version = PlatformVersion,
                     health_status = ok},
    Job = #job{command = Command, deliv_ssh_job_pid = deliv_pid},
    NewRunner = Runner#runner{health_status = ok, job = Job#job{jobs_command_pid = pid}},

    hoax:expect(receive
                    jobs_queue:find_pending_job(Os, PlatformFamily, Platform, PlatformVersion) -> Job;
                    deliv_ssh_job:job_submitted(deliv_pid, <<"some_host">>) -> ignored;
                    jobs_command:start(Runner, Command) -> {ok, pid};
                    chef_utils:send_after(1000, self(), request_job) -> unused;
                     deliv_event:publish(runner_state_changed, NewRunner) -> unused
                end),

    Actual = jobs_runner:handle_info(request_job, Runner),
    ?assertEqual({noreply, NewRunner}, Actual),
    ?verifyAll.

handle_info_request_job_tuple_when_no_pending_job_does_not_crash() ->
    Hostname = <<"some_host">>,
    [Os, PlatformFamily, Platform, PlatformVersion] = [<<"linux">>, <<"debian">>, <<"ubuntu">>, <<"15.04">>],
    Runner = #runner{hostname = Hostname,
                     private_key = private_key,
                     job = undefined,
                     os = Os,
                     platform_family = PlatformFamily,
                     platform = Platform,
                     platform_version = PlatformVersion,
                     health_status = undefined},

    hoax:expect(receive
                    jobs_queue:find_pending_job(Os, PlatformFamily, Platform, PlatformVersion) -> not_found;
                    chef_utils:send_after(1000, self(), request_job) -> unused
                end),

    Actual = jobs_runner:handle_info(request_job, Runner),
    ?assertEqual({noreply, Runner}, Actual),
    ?verifyAll.

handle_info_request_job_when_health_status_pending_does_not_pick_up_job() ->
    Runner = #runner{health_status = pending},

    hoax:expect(receive
                    chef_utils:send_after(1000, self(), request_job) -> unused
                end),

    Actual = jobs_runner:handle_info(request_job, Runner),
    ?assertEqual({noreply, Runner}, Actual),
    ?verifyAll.

handle_info_request_job_when_health_error_pending_does_not_pick_up_job() ->
    Runner = #runner{health_status = error},

    hoax:expect(receive
                    chef_utils:send_after(1000, self(), request_job) -> unused
                end),

    Actual = jobs_runner:handle_info(request_job, Runner),
    ?assertEqual({noreply, Runner}, Actual),
    ?verifyAll.

handle_info_request_job_when_job_in_flight_with_health_runner_restarts_erlang_send_after() ->
    Hostname = <<"some_host">>,
    Runner = #runner{health_status = ok,
                     hostname = Hostname,
                     private_key = private_key,
                     job = #job{}},

    hoax:expect(receive
                    chef_utils:send_after(1000, self(), request_job) -> unused
                end),

    Actual = jobs_runner:handle_info(request_job, Runner),
    ?assertEqual({noreply, Runner}, Actual),
    ?verifyAll.

format_status_when_called_for_abnormal_termination_redacts_job_runner_private_key() ->
    Hostname = <<"runner1.example.com">>,
    State = #runner{hostname = Hostname,
                    private_key = jobs_test_utils:test_private_key()},
    OutputStatus = [{data, [{"Runner", State#runner{private_key = undefined}}]}],
    ?assertEqual(OutputStatus, jobs_runner:format_status(terminate, [[], State])).

format_status_when_called_for_normal_termination_redacts_job_runner_private_key() ->
    Hostname = <<"runner1.example.com">>,
    State = #runner{hostname = Hostname,
                    private_key = jobs_test_utils:test_private_key()},
    OutputStatus = [{data, [{"Runner", State#runner{private_key = undefined}}]}],
    ?assertEqual(OutputStatus, jobs_runner:format_status(normal, [[], State])).

subscribe_runner_events_calls_deliv_event_with_a_list_of_events() ->
    hoax:expect(receive
                     deliv_event:subscribe([runner_state_changed, runner_stopped]) -> true
                end),

    ?assertEqual(true, jobs_runner:subscribe_runner_events()),
    ?verifyAll.

terminate_sends_runner_stopped_event() ->
    hoax:expect(receive
                     deliv_event:publish(runner_stopped, runner) -> unused
                end),

    Actual = jobs_runner:terminate(reason, runner),

    ?assertEqual({ok, runner}, Actual),
    ?verifyAll.

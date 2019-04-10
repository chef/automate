-module(deliv_push_job_unit_tests).

-include_lib("hoax/include/hoax.hrl").
-include("../../src/deliverance_types.hrl").

-compile(export_all).

interrogate_quorum_failed_fixture_test_() ->
    hoax:fixture(?MODULE, "interrogate_quorum_failed").

compute_restart_state_fixture_test_() ->
    hoax:fixture(?MODULE, "compute_restart_state").

do_next_action_fixture_test_() ->
    hoax:fixture(?MODULE, "do_next_action").

interrogate_quorum_failed_when_nacked_should_return_start() ->
    Node = {<<"nacked">>, [mocked_name]},
    Result = deliv_push_job:interrogate_quorum_failed(fakeid, [Node]),
    ?assertEqual(start, Result).

interrogate_quorum_failed_when_unavailable_should_return_start() ->
    Node = {<<"unavailable">>, [mocked_name]},
    Result = deliv_push_job:interrogate_quorum_failed(fakeid, [Node]),
    ?assertEqual(restart, Result).

interrogate_quorum_failed_when_unhandled_should_return_finished() ->
    Node = {<<"somethingelse">>, [mocked_name]},
    Result = deliv_push_job:interrogate_quorum_failed(fakeid, [Node]),
    ?assertEqual(finished, Result).

interrogate_quorum_failed_when_other_should_throw_unhandled_status() ->
    Node = [broken],
    ?assertExit({unhandled_status, Node},
                deliv_push_job:interrogate_quorum_failed(fakeid, Node)).



compute_restart_state_returns_status_interval_for_status() ->
    Count = 3,
    {Command, ActualIntervalKey, StartCount} = deliv_push_job:compute_restart_state(status, Count),
    ?assertEqual(push_jobs_status_interval, ActualIntervalKey),
    ?assertEqual(Count, StartCount),
    ?assertEqual(status, Command).

compute_restart_state_returns_retry_interval_for_start() ->
    {Command, ActualIntervalKey, StartCount} = deliv_push_job:compute_restart_state(start, 3),
    ?assertEqual(push_jobs_retry_interval, ActualIntervalKey),
    ?assertEqual(1, StartCount),
    ?assertEqual(start, Command).

compute_restart_state_returns_count_plus_one_for_restart() ->
    Count = 3,
    {Command, ActualIntervalKey, StartCount} = deliv_push_job:compute_restart_state(restart, Count),
    ?assertEqual(push_jobs_retry_interval, ActualIntervalKey),
    ?assertEqual(Count+1, StartCount),
    ?assertEqual(start, Command).


do_next_action_sends_phase_finished_for_finished() ->
    State = #state{finished=false, phase_pid=fake_pid},
    hoax:mock(deliv_phase,
            ?expect(finished,
                    ?withArgs([fake_pid]),
                    ?andReturn(ignored))),

    NewState = deliv_push_job:do_next_action(State, finished),
    ?assertEqual(true, NewState#state.finished).

 do_next_action_sends_function_request_with_delay() ->
    State = #state{times_started=3},
    application:set_env(delivery, push_jobs_status_interval, 1),
    application:set_env(delivery, push_jobs_max_retries, 10),

    NewState = deliv_push_job:do_next_action(State, status),
    ?assertEqual(State, NewState).

do_next_action_with_start_sets_times_started_to_1() ->
    State = #state{},
    application:set_env(delivery, push_jobs_retry_interval, 1),
    application:set_env(delivery, push_jobs_max_retries, 10),

    NewState = deliv_push_job:do_next_action(State, start),
    ?assertEqual(1, NewState#state.times_started).

do_next_action_retry_increments_retry_count() ->
    State = #state{times_started=2},
    application:set_env(delivery, push_jobs_retry_interval, 1),
    application:set_env(delivery, push_jobs_max_retries, 10),

    NewState = deliv_push_job:do_next_action(State, restart),
    ?assertEqual(3, NewState#state.times_started).

do_next_action_when_we_hit_max_retries_set_finished_to_true() ->
    State = #state{finished=false, times_started=10},
    application:set_env(delivery, push_jobs_max_retries, 10),

    NewState = deliv_push_job:do_next_action(State, restart),
    ?assertEqual(true, NewState#state.finished).

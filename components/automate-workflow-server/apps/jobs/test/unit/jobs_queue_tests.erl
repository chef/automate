-module(jobs_queue_tests).

-include("jobs_types.hrl").
-include_lib("hoax/include/hoax.hrl").

-compile(export_all).

jobs_queue_test_() ->
    [
     hoax:fixture(?MODULE, "init_"),
     hoax:fixture(?MODULE, "handle_call_find_pending_job_"),
     hoax:fixture(?MODULE, "handle_call_find_job_"),
     hoax:fixture(?MODULE, "handle_call_remove_job_"),
     hoax:fixture(?MODULE, "handle_call_state_"),
     hoax:fixture(?MODULE, "handle_info_enqueue_job_")
    ].

init_sets_initial_state_to_empty_queue() ->
    hoax:expect(receive
                    deliv_ssh_job:subscribe_job_events() -> true
                end),

    Actual = jobs_queue:init([]),
    ?assertEqual({ok, []}, Actual),
    ?verifyAll.

handle_call_find_pending_job_when_no_matching_job_in_queue_returns_not_found() ->
    hoax:expect(receive
                    jobs_criteria_queue:find_job_for_runner(pid, runner_properties, queue) -> {not_found, queue}
                end),

    Actual = jobs_queue:handle_call({find_pending_job, runner_properties}, {pid, tag}, queue),
    ?assertEqual({reply, not_found, queue}, Actual).

handle_call_find_pending_job_when_queue_returns_matching_job_returns_job() ->
    hoax:expect(receive
                    jobs_criteria_queue:find_job_for_runner(pid, runner_properties, queue) -> {job, new_queue};
                    deliv_event:publish(update_job_queue, new_queue) -> ignored
                end),

    Actual = jobs_queue:handle_call({find_pending_job, runner_properties}, {pid, tag}, queue),
    ?assertEqual({reply, job, new_queue}, Actual),
    ?verifyAll.

handle_call_find_job_returns_response() ->
    Id = <<"jobid">>,
    hoax:expect(receive
                    jobs_criteria_queue:find_by_id(Id, queue) -> response
                end),

    Actual = jobs_queue:handle_call({find_job, Id}, from, queue),
    ?assertEqual({reply, response, queue}, Actual),
    ?verifyAll.

handle_call_remove_job_returns_response() ->
    Id = <<"jobid">>,
    hoax:expect(receive
                    jobs_criteria_queue:remove_job_by_id(Id, queue) -> {job, new_queue};
                    deliv_event:publish(update_job_queue, new_queue) -> ignored
                end),

    Actual = jobs_queue:handle_call({remove_job, Id}, from, queue),
    ?assertEqual({reply, job, new_queue}, Actual),
    ?verifyAll.

handle_call_state_returns_the_queue() ->
    ?assertEqual({reply, queue, queue}, jobs_queue:handle_call(state, from, queue)).

handle_info_enqueue_job_on_notification_queues_job() ->
    Cmd = <<"delivery-cmd">>,
    Pid = list_to_pid("<0.2.4>"),
    JobId = <<"jobid">>,
    JobCriteria = job_criteria,
    DelivChangeInfo = change_info,
    Job = #job{id = JobId,
               job_criteria = JobCriteria,
               command = Cmd,
               deliv_ssh_job_pid = Pid,
               deliv_change_info = DelivChangeInfo,
               status = pending
              },

    hoax:expect(receive
                    jobs_criteria_queue:queue_job(Job, queue) -> new_queue;
                    deliv_event:publish(update_job_queue, new_queue) -> ignored
                end),

    Actual = jobs_queue:handle_info({Pid, enqueue_job, {Cmd, JobCriteria, JobId, DelivChangeInfo}}, queue),
    ?assertEqual({noreply, new_queue}, Actual),
    ?verifyAll.

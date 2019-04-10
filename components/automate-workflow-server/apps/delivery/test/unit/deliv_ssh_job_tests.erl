%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-

-module(deliv_ssh_job_tests).

-include_lib("hoax/include/hoax.hrl").
-include("../src/deliv_phase.hrl").

-compile(export_all).

jobs_runner_registry_test_() ->
    [
     hoax:fixture(?MODULE, "init_"),
     hoax:fixture(?MODULE, "handle_call_start_"),
     hoax:fixture(?MODULE, "handle_call_job_submitted_"),
     hoax:fixture(?MODULE, "handle_call_job_finished_"),
     hoax:fixture(?MODULE, "handle_call_job_failed_"),
     hoax:fixture(?MODULE, "handle_call_job_canceled_"),
     hoax:fixture(?MODULE, "subscribe_job_events_")
    ].

init_sets_state_record() ->
    JobId = <<"jobid">>,
    PhasePid = pid,
    Criteria = criteria,
    Command = <<"delivery-cmd base64">>,
    ChangeRecord = #deliv_change_info{
                      title = change_title,
                      submitted_at = change_submitted_at,
                      stage = stage_name,
                      phase = phase_name
                     },

    Expected = #deliv_ssh_job_state{id = JobId,
                                    command = Command,
                                    criteria = Criteria,
                                    phase_pid = PhasePid,
                                    change_info = ChangeRecord
                                   },

    Actual = deliv_ssh_job:init({PhasePid, Criteria, JobId, Command, ChangeRecord}),
    ?assertEqual({ok, Expected}, Actual),
    ?verifyAll.

subscribe_job_events_subscribes_caller_to_events() ->
    hoax:expect(receive
                    deliv_event:subscribe([enqueue_job]) -> true
                end),

    ?assertEqual(true, deliv_ssh_job:subscribe_job_events()),
    ?verifyAll.

handle_call_start_publish_job_ready_event_with_command() ->
    Command = <<"my command">>,
    Id = <<"undefined">>,
    State = #deliv_ssh_job_state{id = Id,
                                 command = Command,
                                 criteria = criteria,
                                 change_info = change_info
                                },
    hoax:expect(receive
                    deliv_event:publish(enqueue_job, {Command, criteria, Id, change_info}) -> ignored
                end),

    Actual = deliv_ssh_job:handle_call(start, from, State),
    ?assertEqual({reply, ok, State}, Actual),
    ?verifyAll.

handle_call_job_submitted_builds_phase_job_status_and_calls_deliv_phase_update() ->
    State = #deliv_ssh_job_state{phase_pid = some_phase_pid},
    PhaseState = #phase_job_status{started = true,
                                   status = <<"running">>,
                                   node = hostname},
    hoax:expect(receive
                    deliv_phase:update(some_phase_pid, PhaseState) -> ignored
                end),

    Actual = deliv_ssh_job:handle_call({job_submitted, hostname}, from, State),
    ?assertEqual({reply, ok, State}, Actual),
    ?verifyAll.

handle_call_job_failed_calls_deliv_phase_failed() ->
    Hostname = <<"my.runner">>,
    Output = <<"delivery-cmd: no such file or directory">>,
    State = #deliv_ssh_job_state{phase_pid = pid},
    hoax:expect(receive
                    deliv_phase:failed(pid, Hostname, Output) -> ignored
                end),

    Actual = deliv_ssh_job:handle_call({job_failed, Hostname, Output}, from, State),
    ?assertEqual({reply, ok, State}, Actual),
    ?verifyAll.

handle_call_job_canceled_calls_deliv_phase_canceled() ->
    State = #deliv_ssh_job_state{phase_pid = pid},
    hoax:expect(receive
                    deliv_phase:canceled(pid) -> ignored
                end),

    Actual = deliv_ssh_job:handle_call(job_canceled, from, State),
    ?assertEqual({reply, ok, State}, Actual),
    ?verifyAll.

handle_call_job_finished_calls_only_logs_that_ssh_has_succeeded() ->
    hoax:expect(receive
                    chef_log:debug("SSH run has finished successfully") -> ignored
                end),

    Actual = deliv_ssh_job:handle_call(job_finished, from, state),
    ?assertEqual({reply, ok, state}, Actual),
    ?verifyAll.

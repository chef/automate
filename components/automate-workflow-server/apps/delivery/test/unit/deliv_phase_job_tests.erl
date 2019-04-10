-module(deliv_phase_job_tests).

-include_lib("hoax/include/hoax.hrl").

-include("../../src/deliv_phase.hrl").
-include("../../src/deliverance_types.hrl").

-compile([export_all]).

phase_job_test_() ->
    [
     eunit_sugar:fixture(?MODULE, stop_),
     eunit_sugar:fixture(?MODULE, status_),
     hoax:fixture(?MODULE, dispatch_)
    ].

stop_forwards_msg_to_push_jobs_test() ->
    hoax:test(fun() ->
        Module = deliv_push_job,
        Pid = list_to_pid("<0.1.0>"),
        PhaseRecord = #deliv_phase{ phase_job_pid = Pid },
        hoax:mock(Module,
                  ?expect(stop,
                          ?withArgs([Pid]),
                          ?andReturn(ok))),

        ?assertEqual(ok, deliv_phase_job:stop(Module, PhaseRecord)),
        ?verifyAll
    end).

status_from_running_push_job_returns_phase_job_status_test() ->
    Node = <<"build_node">>,
    PushJobStatus = #push_job_status{
        status = <<"running">>,
        nodes = {[{<<"running">>, [Node]}]}
    },
    BuildJobStatus = #phase_job_status{
        started = true,
        status = <<"running">>,
        node = Node
    },
    ?assertEqual(BuildJobStatus, deliv_phase_job:status(PushJobStatus)).

status_from_non_running_push_job_returns_phase_job_status_test() ->
    PushJobStatus = #push_job_status{
        status = <<"voting">>,
        nodes = {[]}
    },
    BuildJobStatus = #phase_job_status{
        started = false,
        status = <<"voting">>,
        node = undefined
    },
    ?assertEqual(BuildJobStatus, deliv_phase_job:status(PushJobStatus)).

dispatch_when_job_dispatch_version_is_v1_calls_job_command_and_uses_deliv_push_job() ->
    Stage = stage,
    Phase = phase,
    Change = change,
    Patchset = patchset,
    DispatchVersion = ?JOB_DISPATCH_V1,
    Timeout = 5000,
    PhaseRecord = #deliv_phase{stage_run = Stage,
                               phase_run = Phase,
                               change = Change,
                               patchset = Patchset,
                               job_dispatch_version = DispatchVersion,
                               timeout = Timeout},
    hoax:expect(receive
                    deliv_phase_run:getval(search_query, Phase) -> <<"search_query">>;
                    deliv_job_command:command(Stage,
                                              Phase,
                                              Change,
                                              Patchset,
                                              DispatchVersion,
                                              Timeout) -> command;
                    deliv_push_job:start_link(self(), "search_query", command) -> {ok, pid};
                    deliv_push_job:start(pid) -> ok
                end),

    ?assertEqual({ok, pid}, deliv_phase_job:dispatch(PhaseRecord)),
    ?verifyAll.

dispatch_when_job_dispatch_version_is_v2_calls_job_command_and_uses_deliv_ssh_job_and_passes_stage_phase_and_change() ->
    Stage = stage,
    Phase = phase,
    Change = change,
    Patchset = patchset,
    JobId = <<"generated jobid">>,
    ProjName = proj_name,
    OrgName = org_name,
    DispatchVersion = ?JOB_DISPATCH_V2,
    Timeout = 5000,
    PhaseRecord = #deliv_phase{stage_run = Stage,
                               phase_run = Phase,
                               change = Change,
                               patchset = Patchset,
                               job_dispatch_version = DispatchVersion,
                               criteria = some_criteria,
                               timeout = Timeout},
    ExpectedChangeRecord = #deliv_change_info{
                              id = change_id,
                              project = ProjName,
                              title = change_title,
                              org = OrgName,
                              submitted_at = change_submitted_at,
                              stage = stage_name,
                              phase = phase_name
                             },
    hoax:expect(receive
                    deliv_job_command:command(Stage,
                                              Phase,
                                              Change,
                                              Patchset,
                                              DispatchVersion,
                                              Timeout) -> command;
                    deliv_change:getval(id, Change) -> change_id;
                    deliv_change:scoping_names(change_id) -> [ent, OrgName, ProjName, pipe];
                    deliv_stage_run:getval(id, Stage) -> stage_id;
                    deliv_phase_run:getval(id, Phase) -> phase_id;
                    deliv_stage_run:getval(stage, Stage) -> stage_name;
                    deliv_phase_run:getval(phase, Phase) -> phase_name;
                    deliv_change:getval(title, Change) -> change_title;
                    deliv_change:getval(submitted_at, Change) -> change_submitted_at;
                    deliv_job_command:generate_id(stage_id, phase_id, change_id) -> JobId;
                    deliv_ssh_job:start_link(self(), some_criteria, JobId, command, ExpectedChangeRecord) -> {ok, pid};
                    deliv_ssh_job:start(pid) -> ok
                end),
    ?assertEqual({ok, pid}, deliv_phase_job:dispatch(PhaseRecord)),
    ?verifyAll.

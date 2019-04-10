%% @doc This handles push job dispatching and state management.
-module(deliv_phase_job).

-include("deliv_phase.hrl").
-include("deliverance_types.hrl").

-export([
         dispatch/1,
         stop/1,
         status/1
        ]).

-ifdef(TEST).
-export([
         stop/2
        ]).
-endif.

-spec dispatch(#deliv_phase{}) -> {ok, pid()}.
dispatch(#deliv_phase{job_dispatch_version = ?JOB_DISPATCH_V1,
                      stage_run = StageRun,
                      phase_run = PhaseRun,
                      patchset = Patchset,
                      change = Change,
                      timeout = Timeout}) ->
    SearchQuery = chef_utils:to_str(deliv_phase_run:getval(search_query, PhaseRun)),
    Command = deliv_job_command:command(StageRun,
                                        PhaseRun,
                                        Change,
                                        Patchset,
                                        ?JOB_DISPATCH_V1,
                                        Timeout),
    {ok, JobPid} = deliv_push_job:start_link(self(), SearchQuery, Command),
    ok = deliv_push_job:start(JobPid),
    {ok, JobPid};
dispatch(#deliv_phase{job_dispatch_version = ?JOB_DISPATCH_V2,
                      criteria = Criteria,
                      stage_run = StageRun,
                      phase_run = PhaseRun,
                      patchset = Patchset,
                      change = Change,
                      timeout = Timeout}) ->
    ChangeId = deliv_change:getval(id, Change),
    [_EntName, OrgName, ProjName, _PipeName] = deliv_change:scoping_names(ChangeId),
    Command = deliv_job_command:command(StageRun,
                                        PhaseRun,
                                        Change,
                                        Patchset,
                                        ?JOB_DISPATCH_V2,
                                        Timeout),
    JobId = deliv_job_command:generate_id(deliv_stage_run:getval(id, StageRun),
                                          deliv_phase_run:getval(id, PhaseRun),
                                          ChangeId),
    ChangeInfo = #deliv_change_info{
                    id = ChangeId,
                    project = ProjName,
                    org = OrgName,
                    title = deliv_change:getval(title, Change),
                    submitted_at = deliv_change:getval(submitted_at, Change),
                    stage = deliv_stage_run:getval(stage, StageRun),
                    phase = deliv_phase_run:getval(phase, PhaseRun)
                   },
    {ok, JobPid} = deliv_ssh_job:start_link(self(), Criteria, JobId, Command, ChangeInfo),
    ok = deliv_ssh_job:start(JobPid),
    {ok, JobPid}.

-spec stop(#deliv_phase{}) -> ok.
stop(#deliv_phase{job_dispatch_version = ?JOB_DISPATCH_V1} = State) ->
    stop(deliv_push_job, State);
stop(#deliv_phase{job_dispatch_version = ?JOB_DISPATCH_V2} = State) ->
    stop(deliv_ssh_job, State).

-spec stop(atom(), #deliv_phase{}) -> ok.
stop(Module, #deliv_phase{phase_job_pid = PhaseJobPid}) ->
    Module:stop(PhaseJobPid).

-spec status(#push_job_status{}) -> #phase_job_status{}.
status(#push_job_status{status = JobStatus} = PushJobStatus) ->
    #phase_job_status{
        started = is_started(PushJobStatus),
        status = JobStatus,
        node = extract_node(PushJobStatus)
    }.

%% @private
%% @doc Extracts the node name from push job's response
%% See https://docs.getchef.com/push_jobs.html#jobs-id
%% Note that this assumes that we only dispatch to a single node!
-spec extract_node(#push_job_status{}) -> binary().
extract_node(#push_job_status{nodes=Nodes}) ->
    Keys = [<<"aborted">>, <<"complete">>, <<"crashed">>, <<"nacked">>,
            <<"new">>, <<"ready">>, <<"running">>, <<"unavailable">>,
            <<"failed">>,<<"succeeded">>],
    extract_node(Nodes, Keys).

%% @doc Returns true iff a build node has been chosen yet
-spec is_started(#push_job_status{}) -> boolean().
is_started(#push_job_status{status=PushJobStatusName}) ->
    not(lists:member(PushJobStatusName, [<<"quorum_failed">>, <<"voting">>])).

%% @private
extract_node(Ejson, [Key | Rest]) ->
    case ej:get([Key], Ejson) of
        undefined -> extract_node(Ejson, Rest);
        [Node] -> Node
    end;
extract_node(_Ejson, []) ->
    undefined.

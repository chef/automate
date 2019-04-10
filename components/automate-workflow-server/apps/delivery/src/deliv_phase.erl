%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Adam Jacob <adam@opscode.com>
%% Copyright 2013 Opscode, Inc. All Rights Reserved.
%%

%% @doc Common FSM for all phases
-module(deliv_phase).
-behaviour(gen_fsm).

-include("deliverance_types.hrl").

%% Generic behaviour callbacks
-export([init/1, handle_event/3, terminate/3,
         code_change/4, handle_info/3, handle_sync_event/4]).

%% FSM States
-export([idle/2, skip/2, waiting_worker/2, running/2, waiting_log/2]).

%% Delivery API
-export([start_link/11, run/1, subscribe_phase_events/0, subscribe_phase_events/2]).

%% Used by deliv_push_jobs to update the FSM
-export([no_worker/1, update/2, finished/1, failed/3, canceled/1]).

%% In order to feasibly write unit tests for this module, its #deliv_phase record
%% definition has been extracted to a HRL. If/when it becomes possible to run
%% embedded eunit tests in our development tooling, those tests and this record
%% can be merged back into this module.
-include("deliv_phase.hrl").

-define(TIMEOUT, delivery_app:get_env(phase_job_confirmation_timeout)).

%%%
%%% API funs
%%%

%% @doc Create the deliv_phase worker for old-school push jobs dispatch.
-spec start_link(d_change(), d_patchset(), d_stage_run(), pid(),
                 Phase :: binary(), SearchQuery :: binary() | undefined, SearchDescription :: binary() | undefined,
                 boolean(), binary(), deliv_ssh_job_criteria() | undefined,
                 integer() | undefined) -> {ok, pid()} | {error, term()}.
start_link(Change, Patchset, StageRun, StageRunPid, Phase, SearchQuery, SearchDescription, Skip, JobDispatchVersion, Criteria, Timeout) ->
    gen_fsm:start_link(?MODULE, {{Change, Patchset, StageRun, StageRunPid, Phase, SearchQuery, SearchDescription, JobDispatchVersion, Criteria, Timeout}, Skip}, []).

%% @doc API Trigger - Start the deliv_phase worker.
-spec run(pid()) -> ok.
run(Pid) ->
    gen_fsm:send_event(Pid, start).

%% @doc API Trigger from Push Jobs - There are no available workers.
-spec no_worker(pid()) -> ok.
no_worker(Pid) ->
    gen_fsm:send_event(Pid, no_worker).

%% @doc API Trigger from Push Jobs - Update the push jobs status.
-spec update(pid(), #push_job_status{} | #phase_job_status{}) -> ok.
update(Pid, #push_job_status{} = Status) ->
    PhaseJobStatus = deliv_phase_job:status(Status),
    gen_fsm:send_event(Pid, {job_updated, PhaseJobStatus});
update(Pid, #phase_job_status{} = Status) ->
    gen_fsm:send_event(Pid, {job_updated, Status}).

%% @doc API Trigger from Push Jobs - Push job has finished.
-spec finished(pid()) -> ok.
finished(Pid) ->
    gen_fsm:send_event(Pid, job_finished).

%% @doc API Trigger from SSH Jobs - Job failed to dispatch (so no logs are to
%% be expected to come from this dispatched job)
-spec failed(pid(), binary(), binary()) -> ok.
failed(Pid, Hostname, Output) ->
    gen_fsm:send_event(Pid, {job_failed, Hostname, Output}).

%% @doc API Trigger from SSH Jobs - Job cancelled, so set the phase state to "failed".
-spec canceled(pid()) -> ok.
canceled(Pid) ->
    gen_fsm:send_event(Pid, job_canceled).

%% @doc Subscribe to events triggered by FSM state changes
-spec subscribe_phase_events() -> boolean().
subscribe_phase_events() ->
    deliv_phase_event:subscribe().

-spec subscribe_phase_events(atom(), atom()) -> boolean().
subscribe_phase_events(Action, all) ->
    deliv_phase_event:subscribe(Action, all);
subscribe_phase_events(all, Phase) ->
    deliv_phase_event:subscribe(all, Phase);
subscribe_phase_events(Action, Phase) ->
    deliv_phase_event:subscribe(Action, Phase).


%%%
%%% Internal States Diagram
%%%
%%
%% state           event           next                description
%% ------------------------------------------------------------------
%% skip            start             _finish_
%% idle            start             waiting_worker    initial state
%%                   - search for workers
%%                   - try to actually start the job
%% waiting_worker  update            waiting_worker    waiting for a worker
%%                                   running
%%                 no_worker         _finish_
%%                 job_canceled      _finish_
%%
%% running         job_updated       running           We got an update from the build job. Update the job_status if appropriate
%%                 job_finished      waiting_log       Push job said phase is complete. Wait for confirmation from the phase run logs.
%%                 phase_run/update  running           We got an update from the phase logs. Nothing to do here.
%%                 job_failed        _finish_          SSH job couldn't execute. No phase run logs to be expected.
%%                 job_canceled      _finish_          SSH job canceled by user.
%%                 phase_run/(p|f)   _finish_          Phase run logs said phase is complete. Defer to them and stop the phase.
%%
%% waiting_log     phase_run/(p|f)   _finish_          Got the confirmation from the phase_run logs that phase is complete. Stop the phase
%%                 phase_run/update  waiting_log       More phase run logs continue to come in, still waiting for logs to say the phase is done (with timeout)
%%                 timeout           _finish_          We waited the specified period of time, but no more logs came in. Stop the phase.
%%

%% @doc Skip the phase
-spec skip(start, #deliv_phase{}) -> {stop, normal, #deliv_phase{}}.
skip(start, #deliv_phase{stage_run_pid=StageRunPid,
                         phase_run=PhaseRun} = State) ->
    Msg = <<"Skipped phase">>,
    log(info, State, Msg),

    {ok, NewPhaseRun} = deliv_phase_run:finish(skipped, Msg, PhaseRun),

    %% notify the stage worker
    deliv_stage:phase_finished(StageRunPid, self(), skipped),

    NewState = State#deliv_phase{phase_run=NewPhaseRun},
    publish_phase_event(finished, skipped, undefined, undefined, NewState),
    stop(NewState).

%% @doc Phase has started. Launch the Push Job.
-spec idle(start, #deliv_phase{}) -> {next_state, waiting_worker, #deliv_phase{}}.
idle(start, State) ->
    log(debug, State, "Starting"),

    {ok, PhaseJobPid} = deliv_phase_job:dispatch(State),

    NewState = State#deliv_phase{phase_job_pid=PhaseJobPid},

    publish_phase_event(waiting_for_worker, idle, waiting_for_worker, undefined, NewState),
    next_state(waiting_worker, NewState).

%% @doc Waiting for a build node
-spec waiting_worker({job_updated, #phase_job_status{}} | no_worker | job_canceled, #deliv_phase{})
        -> {next_state, running | waiting_worker, #deliv_phase{}} | {stop, normal, #deliv_phase{}}.
waiting_worker({job_updated, #phase_job_status{status = JobStatus, node = Node, started = true}},
               #deliv_phase{job_dispatch_version = JobDispatchVersion, phase_run = PhaseRun} = State) ->
    DispatchVersion = case JobDispatchVersion of
                        ?JOB_DISPATCH_V1 -> "Push";
                        ?JOB_DISPATCH_V2 -> "SSH";
                        _ -> "Unknown Job Dispatch Version"
                      end,
    log(info, State, "~s job started on ~s", [DispatchVersion, Node]),

    [NewPhaseRun] = deliv_phase_run:start(Node, PhaseRun),

    NewState = State#deliv_phase{phase_run=NewPhaseRun},
    publish_phase_event(started, running, undefined, JobStatus, NewState),
    next_state(running, NewState);
waiting_worker({job_updated, #phase_job_status{started = false}}, State) ->
    next_state(waiting_worker, State);
waiting_worker(job_canceled, #deliv_phase{phase_run=PhaseRun,
                                          stage_run_pid=StageRunPid} = State) ->
    Msg = <<"Job was manually canceled by a user.">>,

    log(error, State, Msg),
    {ok, NewPhaseRun} = deliv_phase_run:finish(failed, Msg, PhaseRun),

    deliv_stage:phase_finished(StageRunPid, self(), failed),

    NewState = State#deliv_phase{phase_run=NewPhaseRun},
    publish_phase_event(finished, failed, job_canceled, undefined, NewState),
    terminate(NewState);
waiting_worker(no_worker, #deliv_phase{phase_run=PhaseRun,
                                       stage_run_pid=StageRunPid} = State) ->
    Msg = <<"Could not start phase run. No build nodes available">>,
    log(error, State, Msg),

    {ok, NewPhaseRun} = deliv_phase_run:finish(failed, Msg, PhaseRun),

    %% notify the stage worker
    deliv_stage:phase_finished(StageRunPid, self(), failed),

    NewState = State#deliv_phase{phase_run=NewPhaseRun},
    publish_phase_event(finished, failed, no_workers, undefined, NewState),
    terminate(NewState).

-spec running(job_finished | {job_updated, #phase_job_status{}} | {job_failed, binary(), binary()} | job_canceled, #deliv_phase{})
        -> {next_state, running, #deliv_phase{}} | {stop, normal, #deliv_phase{}}.
running({job_updated, #phase_job_status{status = JobStatus} = PhaseJobStatus}, #deliv_phase{job_status = JobStatus} = State) ->
    log(debug, State, "Received a status update from push-jobs: ~p", [PhaseJobStatus]),
    next_state(running, State);
running({job_updated, #phase_job_status{status = JobStatus} = PhaseJobStatus}, State) ->
    NewState = State#deliv_phase{job_status = JobStatus},
    log(debug, NewState, "Received a status update from push-jobs: ~p", [PhaseJobStatus]),
    publish_phase_event(updated, running, undefined, JobStatus, NewState),
    next_state(running, NewState);
running({job_failed, Hostname, Output}, #deliv_phase{phase_run=PhaseRun} = State) ->
    Msg = <<"Job dispatch on job runner ", Hostname/binary, " has failed:\n", Output/binary>>,
    log(info, State, Msg),
    {ok, NewPhaseRun} = deliv_phase_run:finish_with_log_append(Msg, PhaseRun),
    complete_phase(State#deliv_phase{phase_run=NewPhaseRun}, failed);
running(job_canceled, #deliv_phase{phase_run=PhaseRun} = State) ->
    Msg = <<"Job was manually canceled by a user.">>,
    log(info, State, Msg),
    {ok, NewPhaseRun} = deliv_phase_run:finish_with_log_append(Msg, PhaseRun),
    complete_phase(State#deliv_phase{phase_run=NewPhaseRun}, failed);
running(job_finished, #deliv_phase{} = State) ->
    Msg = <<"Job dispatch has completed. Waiting for confirmation from run logs">>,
    log(debug, State, Msg),
    next_state(waiting_log, State, ?TIMEOUT).

%% @doc Waiting for phase run logs to confirm phase is complete.
-spec waiting_log(timeout, #deliv_phase{}) -> {stop, normal, #deliv_phase{}}.
waiting_log(timeout, State) ->
    complete_phase(State, failed).

%%
%% FSM Scaffolding
%%

init({StateVars, true}) ->
    ok(skip, init_state(StateVars));
init({StateVars, false}) ->
    ok(idle, init_state(StateVars)).

handle_event(_Event, StateName, State) ->
    next_state(StateName, State).

-ifdef(TEST).
%% For testing purposes, allow us to force the state of the FSM
handle_sync_event({force_state, NewState, NewData}, _,  _, _) ->
    {reply, ok, NewState, NewData}.
-else.
handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.
-endif.

handle_info({_, {phase_run, _PhaseId}, updated}, waiting_log, State) ->
    next_state(waiting_log, State, ?TIMEOUT);
handle_info({_, {phase_run, _PhaseId}, updated}, StateName, State) ->
    next_state(StateName, State);
handle_info({_, {phase_run, _PhaseId}, Status}, _StateName, State) when Status =:= failed; Status =:= passed ->
    complete_phase(State, Status);
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

-ifdef(TEST).
terminate(normal, _, _) ->
    ok;
terminate(Reason, _StateName, _State) ->
    io:format(standard_error, "~p~n", [Reason]),
    ok.
-else.
terminate(_Reason, _StateName, _State) ->
    ok.
-endif.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%% End of FSM funs

%%%
%%% Internals
%%%

%% @private
init_state({Change, Patchset, StageRun, StageRunPid, Phase, SearchQuery, SearchDescription, JobDispatchVersion, Criteria, Timeout}) ->
    StageRunId = deliv_stage_run:getval(id, StageRun),
    ChangeId = deliv_change:getval(id, Change),
    [Ent, Org, Proj, Pipe] = deliv_change:scoping_names(ChangeId),
    Description = case SearchDescription of
        undefined ->
            chef_utils:to_bin("~s - ~s", [Proj, SearchQuery]);
        _ ->
            chef_utils:to_bin("~s - ~s", [Proj, SearchDescription])
        end,

    [PhaseRun] = deliv_phase_run:insert([{stage_run_id, StageRunId},
                                         {phase, Phase},
                                         {search_query, SearchQuery},
                                         {search_description, SearchDescription},
                                         {status, <<"idle">>},
                                         {run_log, <<"Waiting for a worker.">>},
                                         {description, Description}]),

    %% Sub to phase_log notification events to help determine status
    PhaseId = deliv_phase_run:getval(id, PhaseRun),
    deliv_event:subscribe({phase_run, PhaseId}),

    #deliv_phase{change=Change,
                 patchset=Patchset,
                 stage_run=StageRun,
                 stage_run_pid=StageRunPid,
                 phase_run=PhaseRun,
                 job_dispatch_version=JobDispatchVersion,
                 criteria=Criteria,
                 scoping_names=[Ent, Org, Proj, Pipe],
                 timeout = Timeout}.

%% @private
complete_phase(#deliv_phase{stage_run_pid=StageRunPid} = State, Status) ->
    log(info, State, "Finished phase"),

    %% notify the stage worker
    deliv_stage:phase_finished(StageRunPid, self(), Status),

    publish_phase_event(finished, Status, undefined, undefined, State),
    terminate(State).

%% @private
%% @doc Terminates the FSM, and also the push-job worker
-spec terminate(#deliv_phase{}) -> {stop, normal, #deliv_phase{}}.
terminate(State) ->
    ok = deliv_phase_job:stop(State),
    stop(State).

%% @private
%% @doc Utility function to add some meta to log messages
-spec log(lager:log_level(), #deliv_phase{}, binary() | string()) -> _.
log(LogLevel, State, MsgFormat) ->
    log(LogLevel, State, MsgFormat, []).

%% @private
%% @doc Same as `log/3', with some format args
-spec log(lager:log_level(), #deliv_phase{}, binary() | string(), list()) -> _.
log(LogLevel, #deliv_phase{stage_run=StageRun,
                           phase_run=PhaseRun,
                           scoping_names=[Ent, Org, Proj, Pipe]}, MsgFormat, FormatArgs) ->
    Format = "Phase ~s/~s for change ~s on ~s/~s/~s/~s: " ++ chef_utils:to_str(MsgFormat),
    Args = [deliv_stage_run:getval(stage, StageRun),
            deliv_phase_run:getval(phase, PhaseRun),
            deliv_stage_run:getval(change_id, StageRun),
            Ent, Org, Proj, Pipe | FormatArgs],
    chef_log:log(LogLevel, Format, Args).

%% @private
%% @doc Publish a #phase_event
publish_phase_event(Msg, Status, Reason, JobStatus, State) ->
    PhaseEvent = deliv_phase_event:new(Status, Reason, JobStatus, State),
    deliv_phase_event:publish(Msg, PhaseEvent).

%%
%% In order to be able to inspec the FSM state, we use wrappers around the
%% FSM state return values to allow us to send events with the internal state
%% that we can listen for and capture in our tests. This speeds up and
%% increases the reliability of the tests because we can avoid log sleeps.
%%
-ifdef(TEST).
stop(State) ->
    deliv_event:publish(phase_state_event, {none, State}),
    {stop, normal, State}.

ok(NextState, State) ->
    deliv_event:publish(phase_state_event, {NextState, State}),
    {ok, NextState, State}.

next_state(NextState, State) ->
    deliv_event:publish(phase_state_event, {NextState, State}),
    {next_state, NextState, State}.

next_state(NextState, State, Timeout) ->
    deliv_event:publish(phase_state_event, {NextState, State}),
    {next_state, NextState, State, Timeout}.
-else.
stop(State) ->
    {stop, normal, State}.

ok(NextState, State) ->
    {ok, NextState, State}.

next_state(NextState, State) ->
    {next_state, NextState, State}.

next_state(NextState, State, Timeout) ->
    {next_state, NextState, State, Timeout}.
-endif.

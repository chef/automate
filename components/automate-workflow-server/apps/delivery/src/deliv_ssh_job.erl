%% @doc A gen_server that handles communication and state handling between the jobs app
%% and deliv_phase_job / deliv_phase.
-module(deliv_ssh_job).

-include("deliv_phase.hrl").

-behaviour(gen_server).
-export([
         code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         init/1,
         start_link/5,
         terminate/2
        ]).

-export([
         subscribe_job_events/0,
         start/1,
         stop/1,
         job_dispatch_failed/3,
         job_finished/1,
         job_canceled/1,
         job_submitted/2
        ]).

%% the amount of time to spend waiting for a worker and then waiting for the
%% job to finish (in milliseconds).
-define(TIMEOUT, 1000 * delivery_app:get_env(push_jobs_overall_timeout)).

%% API
-spec subscribe_job_events() -> true.
subscribe_job_events() ->
    deliv_event:subscribe([enqueue_job]).

-spec start_link(pid(), deliv_ssh_job_criteria(), binary(), binary(), deliv_change_info()) -> {ok, pid()}.
start_link(PhasePid, Criteria, JobId, Cmd, ChangeInfo) ->
    gen_server:start_link(?MODULE, {PhasePid, Criteria, JobId, Cmd, ChangeInfo}, []).

-spec job_submitted(pid(), binary()) -> ok.
job_submitted(Pid, Hostname) ->
    gen_server:call(Pid, {job_submitted, Hostname}, ?TIMEOUT).

-spec job_dispatch_failed(pid(), binary(), binary()) -> ok.
job_dispatch_failed(Pid, Hostname, Output) ->
    gen_server:call(Pid, {job_failed, Hostname, Output}, ?TIMEOUT).

-spec job_finished(pid()) -> ok.
job_finished(Pid) ->
    gen_server:call(Pid, job_finished, ?TIMEOUT).

-spec job_canceled(pid()) -> ok.
job_canceled(Pid) ->
    gen_server:call(Pid, job_canceled, ?TIMEOUT).

-spec start(pid()) -> ok.
start(Pid) ->
    gen_server:call(Pid, start, ?TIMEOUT).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:call(Pid, stop, ?TIMEOUT).


%% gen_server callbacks
init({PhasePid, Criteria, Id, Cmd, ChangeInfo}) ->
    State = #deliv_ssh_job_state{phase_pid = PhasePid,
                                 criteria = Criteria,
                                 id = Id,
                                 command = Cmd,
                                 change_info = ChangeInfo
                                },
    {ok, State}.

handle_call(start, _From, State = #deliv_ssh_job_state{id = Id,
                                                       command = Command,
                                                       criteria = Criteria,
                                                       change_info = ChangeInfo
                                                      }) ->
    deliv_event:publish(enqueue_job, {Command, Criteria, Id, ChangeInfo}),
    {reply, ok, State};
handle_call(job_finished, _From, State) ->
    chef_log:debug("SSH run has finished successfully"),
    %% nothing to do, the logs trigger state change
    {reply, ok, State};
handle_call({job_failed, Hostname, Output}, _From, State = #deliv_ssh_job_state{phase_pid = Pid}) ->
    chef_log:info("Updating phase job status: failed"),
    deliv_phase:failed(Pid, Hostname, Output), %% running -> _finish_
    {reply, ok, State};
handle_call(job_canceled, _From, State = #deliv_ssh_job_state{phase_pid = Pid}) ->
    deliv_phase:canceled(Pid),
    {reply, ok, State};
handle_call({job_submitted, Hostname}, _From, State = #deliv_ssh_job_state{phase_pid = Pid}) ->
    chef_log:info("Updating phase job status: submitted"),
    PhaseState = #phase_job_status{
                    started = true, %% this triggers waiting_worker -> running state transition
                    status = <<"running">>,
                    node = Hostname
                   },
    deliv_phase:update(Pid, PhaseState),
    {reply, ok, State};
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    {ok, State}.

code_change(_OldVersion, State, _Extras) ->
    {ok, State}.

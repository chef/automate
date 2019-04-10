-module(jobs_runner).

-include("jobs_types.hrl").

-behaviour(gen_server).

-export([
         health/1,
         cancel_job/1,
         fetch_state/1,
         start_link/2,
         format_status/2,
         delete/1,
         subscribe_runner_events/0,
         fetch_ent_hostname/1
        ]).

-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

start_link(?MODULE, Args) ->
    gen_server:start_link(?MODULE, Args, []).

-spec subscribe_runner_events() -> true.
subscribe_runner_events() ->
    deliv_event:subscribe([runner_state_changed, runner_stopped]).

-spec health(pid()) -> jobs_runner().
health(Pid) ->
    gen_server:call(Pid, health).

-spec cancel_job(pid()) -> ok.
cancel_job(Pid) ->
    gen_server:call(Pid, cancel_job).

-spec fetch_state(pid()) -> jobs_runner().
fetch_state(Pid) ->
    gen_server:call(Pid, fetch_state).

-spec fetch_ent_hostname(pid()) -> {binary(), binary()}.
fetch_ent_hostname(Pid) ->
    gen_server:call(Pid, fetch_ent_hostname).

-spec delete(pid()) -> ok | {error, any()}.
delete(Pid) ->
    gen_server:call(Pid, delete).

init([EntName, Hostname, Os, PlatformFamily, Platform, PlatformVersion]) ->
    process_flag(trap_exit, true),
    Ret = case jobs_runner_db:fetch_by_name(EntName, Hostname, self()) of
              [] -> insert(EntName, Hostname, Os, PlatformFamily, Platform, PlatformVersion);
              {error, Error} -> {error, Error};
              [Runner] -> {ok, Runner}
          end,
    publish_unless_error(Ret),
    chef_utils:send_after(?RUNNER_LOOP_INTERVAL, self(), request_job),
    Ret.

handle_cast(Msg, State = #runner{job = Job}) ->
    chef_log:error("job_runner unknown cast ~p, Job ~p", [Msg, Job]),
    {noreply, State}.

handle_info({'EXIT', _From, normal}, State) ->
    {noreply, State};
handle_info({'EXIT', _From, {shutdown, {ExitStatus, Output}}}, State = #runner{job = #health_job{}}) ->
    process_health_job_shutdown(State, ExitStatus, Output);
handle_info({'EXIT', _From, Reason}, State = #runner{job = #health_job{}}) ->
    chef_log:error("jobs_runner health check caught exit with reason: ~p", [Reason]),
    Msg = <<"The erlang process running the health check died unexpectedly.">>,
    {noreply, set_health_check_result(State, error, Msg)};
handle_info({'EXIT', _From, {shutdown, {Reason, Details}}},
            State = #runner{hostname = Hostname,
                            job = #job{
                                     id = JobId,
                                     deliv_ssh_job_pid = DelivSshJobPid
                                    }
                           }) ->
    process_job_shutdown(State, Reason, Details, JobId, DelivSshJobPid, Hostname);
handle_info({'EXIT', From, Reason}, State) ->
    chef_log:error("Caught unexpected jobs_command EXIT signal from ~p with reason ~p", [From, Reason]),
    {noreply, State};
handle_info(request_job, #runner{health_status = HealthStatus} = State) when HealthStatus == pending;
                                                                             HealthStatus == error ->
    chef_utils:send_after(?RUNNER_LOOP_INTERVAL, self(), request_job),
    {noreply, State};
handle_info(request_job, #runner{hostname = Hostname,
                                 job = undefined,
                                 os = Os,
                                 platform_family = PlatformFamily,
                                 platform = Platform,
                                 platform_version = PlatformVersion} = State) ->
    NewState = case jobs_queue:find_pending_job(Os, PlatformFamily, Platform, PlatformVersion) of
                   not_found ->
                       State;
                   Job = #job{command = Command, deliv_ssh_job_pid = DelivSshJobPid} ->
                       deliv_ssh_job:job_submitted(DelivSshJobPid, Hostname),
                       {ok, Pid} = jobs_command:start(State, Command),
                       UpdatedState = State#runner{job = Job#job{jobs_command_pid = Pid}},
                       deliv_event:publish(runner_state_changed, UpdatedState),
                       UpdatedState
               end,
    chef_utils:send_after(?RUNNER_LOOP_INTERVAL, self(), request_job),
    {noreply, NewState};
handle_info(request_job, State) ->
    chef_utils:send_after(?RUNNER_LOOP_INTERVAL, self(), request_job),
    {noreply, State};
handle_info(_Msg, State) ->
    {noreply, State}.

handle_call(health, _From, State = #runner{job=#health_job{}}) ->
    {reply, redact_key(State), State};
handle_call(health, _From, State = #runner{job=#job{}}) ->
    NewState = State#runner{health_status = undefined,
                            health_output = <<"Health check blocked on running job.">>
                           },
    {reply, redact_key(NewState), State};
handle_call(health, _From, State) ->
    jobs_command:start(State, <<"ls -al 2>&1 > /dev/null && echo 'Success!'">>),
    NewState = State#runner{job = #health_job{},
                            health_status = pending,
                            health_output = undefined
                           },
    RedactedState = redact_key(NewState),
    deliv_event:publish(runner_state_changed, RedactedState),
    {reply, RedactedState, NewState};
handle_call(fetch_state, _From, State) ->
    {reply, redact_key(State), State};
handle_call(fetch_ent_hostname, _From, State) ->
    {reply, {State#runner.enterprise_name, State#runner.hostname}, State};
handle_call(delete, _From, State) ->
    case delete_from_db(State) of
        ok -> {reply, ok, State};
        {error, _} = Error -> {reply, Error, State}
    end;
handle_call(cancel_job, _From, State = #runner{job = #job{jobs_command_pid = JobsCommandPid}}) ->
    jobs_command:cancel(JobsCommandPid),
    {reply, ok, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    chef_log:debug("Runner stopped with state: ~p", [State]),
    deliv_event:publish(runner_stopped, State),
    {ok, State}.

code_change(_OldVersion, State, _Extras) ->
    {ok, State}.

%% Private Functions

-spec redact_key(jobs_runner()) -> jobs_runner().
redact_key(#runner{} = Runner) -> Runner#runner{private_key = undefined}.

-spec insert(binary(), binary(), binary(), binary(), binary(), binary()) -> {ok, jobs_runner()} | {error, any()}.
insert(EntName, Hostname, Os, PlatformFamily, Platform, PlatformVersion) ->
    case jobs_runner_db:insert(EntName, jobs_key:hydrate(#runner{
                                                            hostname = Hostname,
                                                            pid = self(),
                                                            os = Os,
                                                            platform_family = PlatformFamily,
                                                            platform = Platform,
                                                            platform_version = PlatformVersion
                                                           })) of
        {error, _} = Error -> Error;
        [JobRunner] -> {ok, JobRunner}
    end.

publish_unless_error({error, _}) ->
    do_nothing;
publish_unless_error({ok, Runner}) ->
    deliv_event:publish(runner_state_changed, redact_key(Runner)).

-spec delete_from_db(jobs_runner()) -> ok | {error, any()}.
delete_from_db(#runner{} = Runner) ->
    case jobs_runner_db:delete(Runner) of
        {ok, _} -> ok;
        {error, _} = Error -> Error
    end.

-spec format_status(Opt, StatusData) -> Status when
      Opt        :: terminate | normal,
      StatusData :: [PDict | State],
      PDict      :: [term()],
      State      :: jobs_runner(),
      Status     :: term().
format_status(_Opt, [_PDict, #runner{} = State]) ->
    [{data, [{"Runner",State#runner{private_key = undefined}}]}].

set_health_check_result(Runner, Status, Output) ->
    NewRunner = Runner#runner{job = undefined,
                              health_status = Status,
                              health_output = Output},
    deliv_event:publish(runner_state_changed, redact_key(NewRunner)),
    NewRunner.

process_health_job_shutdown(State, 0, Output) ->
    {noreply, set_health_check_result(State, ok, Output)};
process_health_job_shutdown(State, _NonZero, Output) ->
    {noreply, set_health_check_result(State, error, Output)}.

process_job_shutdown(State, error, canceled, JobId, DelivSshJobPid, _Hostname) ->
    jobs_queue:remove_job(JobId),
    deliv_ssh_job:job_canceled(DelivSshJobPid),
    NewState = State#runner{job = undefined},
    deliv_event:publish(runner_state_changed, redact_key(NewState)),
    {noreply, NewState};
process_job_shutdown(State, 0, _Details, JobId, DelivSshJobPid, _Hostname) ->
    jobs_queue:remove_job(JobId),
    deliv_ssh_job:job_finished(DelivSshJobPid),
    NewState = State#runner{job = undefined},
    deliv_event:publish(runner_state_changed, redact_key(NewState)),
    {noreply, NewState};
process_job_shutdown(State, Reason, Details, JobId, DelivSshJobPid, Hostname) ->
    jobs_queue:remove_job(JobId),
    ErrorMessage = erlang:iolist_to_binary([<<"\nExit code: ">>, integer_to_binary(Reason), <<"\n\nDetails:\n">>, Details, <<"\n">>]),
    deliv_ssh_job:job_dispatch_failed(DelivSshJobPid, Hostname, ErrorMessage),
    NewState = State#runner{job = undefined},
    deliv_event:publish(runner_state_changed, redact_key(NewState)),
    {noreply, NewState}.

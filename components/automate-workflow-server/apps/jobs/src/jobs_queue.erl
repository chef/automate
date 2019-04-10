-module(jobs_queue).

-include("jobs_types.hrl").

-behaviour(gen_server).

% Public API
-export([
         start_link/0,
         fetch_state/0,
         find_job_by_id/1,
         remove_job/1,
         find_pending_job/4
        ]).

% gen_server callbacks
-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec fetch_state() -> [job()].
fetch_state() ->
    gen_server:call(?MODULE, state).

-spec find_job_by_id(binary()) -> job() | not_found.
find_job_by_id(Id) ->
    gen_server:call(?MODULE, {find_job, Id}).

-spec remove_job(binary()) -> job() | not_found.
remove_job(Id) ->
    gen_server:call(?MODULE, {remove_job, Id}).

-spec find_pending_job(binary(), binary(), binary(), binary()) -> job() | not_found.
find_pending_job(Os, PlatformFamily, Platform, PlatformVersion) ->
    gen_server:call(?MODULE, {find_pending_job, #runner_properties{os = Os,
                                                                   platform_family = PlatformFamily,
                                                                   platform = Platform,
                                                                   platform_version = PlatformVersion}}).

init([]) ->
    deliv_ssh_job:subscribe_job_events(),
    {ok, []}.

handle_call(state, _From, Jobs) ->
    {reply, Jobs, Jobs};
handle_call({find_pending_job, Properties}, {RunnerPid, _Tag}, Jobs) ->
    {PotentialJob, Jobs2} = jobs_criteria_queue:find_job_for_runner(RunnerPid, Properties, Jobs),
    case PotentialJob of
        not_found ->
            {reply, not_found, Jobs};
        Job ->
            chef_log:debug("Runner with properties ~p assigned job ~p", [Properties, Job]),
            deliv_event:publish(update_job_queue, Jobs2),
            {reply, Job, Jobs2}
    end;
handle_call({find_job, Id}, _From, Jobs) ->
    Reply = jobs_criteria_queue:find_by_id(Id, Jobs),
    {reply, Reply, Jobs};
handle_call({remove_job, Id}, _From, Jobs) ->
    {Result, Jobs2} = jobs_criteria_queue:remove_job_by_id(Id, Jobs),
    deliv_event:publish(update_job_queue, Jobs2),
    {reply, Result, Jobs2};
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({DelivSshJobPid, enqueue_job, {Command, Criteria, Id, DelivChangeInfo}}, Jobs) ->
    Job = #job{id=Id,
               job_criteria=Criteria,
               command=Command,
               deliv_ssh_job_pid=DelivSshJobPid,
               deliv_change_info=DelivChangeInfo,
               status=pending},
    Jobs2 = jobs_criteria_queue:queue_job(Job, Jobs),
    deliv_event:publish(update_job_queue, Jobs2),
    {noreply, Jobs2};
handle_info(Msg, State) ->
    chef_log:error("Received unhandled event: ~p", [Msg]),
    {noreply, State}.

terminate(_Reason, State) ->
    {ok, State}.

code_change(_OldVersion, State, _Extras) ->
    {ok, State}.

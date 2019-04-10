-module(jobs_criteria_queue).

-include("jobs_types.hrl").

-export([
         queue_job/2,
         find_by_id/2,
         find_job_for_runner/3,
         remove_job_by_id/2
        ]).

-spec queue_job(job(), list()) -> list().
queue_job(Job, Jobs) ->
    lists:append(Jobs, [Job]).

-spec find_job_for_runner(pid(), runner_properties(), list()) -> {job(), list()} | {not_found, list()}.
find_job_for_runner(RunnerPid, Properties, JobsQueue) ->
    %% lists:splitwith will keep walking the list while job_not_runnable returns true.
    %% We want to walk the list until we find a job, so when job_not_runnable
    %% returns false, we should stop walking the list via splitwith and
    %% grab the head of the right-splitted list. If splitwith walks the
    %% entire list without job_not_runnable returning false, then there were no
    %% runnable jobs.
    case lists:splitwith(job_not_runnable(Properties), JobsQueue) of
        {_, []} -> {not_found, JobsQueue};
        {Head, [Job|Tail]} ->
            RunningJob = Job#job{status = running,
                                 runner_pid = RunnerPid,
                                 started_running_at = calendar:universal_time()},
            chef_log:debug("Found job to run on runner."),
            {RunningJob, Head ++ [RunningJob] ++ Tail}
    end.

job_not_runnable(Properties) ->
    fun(Job) -> not matching_job(Job, Properties) end.

matching_job(#job{status = running}, _) ->
    false;
matching_job(#job{job_criteria=Criteria}, Properties) ->
    jobs_criteria:match(Criteria, Properties).

-spec find_by_id(binary(), list()) -> job() | not_found.
find_by_id(Id, JobsQueue) ->
    case lists:filter(fun(#job{id = JobId}) -> JobId =:= Id end, JobsQueue) of
        [] -> not_found;
        [Job] -> Job
    end.

-spec remove_job_by_id(binary(), list()) -> {job(), list()} | {not_found, list()}.
remove_job_by_id(Id, JobsList) ->
    case lists:splitwith(fun(#job{id = JobId}) -> JobId =/= Id end, JobsList) of
        {_, []} -> {not_found, JobsList};
        {Head, [Job|Tail]} -> {Job, Head ++ Tail}
    end.

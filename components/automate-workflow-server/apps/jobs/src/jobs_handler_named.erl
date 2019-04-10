-module(jobs_handler_named).
-behaviour(deliv_rest).

-include("jobs_types.hrl").
-include_lib("mixer/include/mixer.hrl").

-export([
         allowed_methods/2,
         content_types_provided/2,
         resource_exists/2,
         delete_resource/2,
         to_json/2
        ]).

-mixin([{deliv_token, [is_authorized/2]},
        {deliv_authz, [forbidden/2]},
        {jobs_rest, [init/3, rest_init/2]}]).

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"DELETE">>], Req, State}.

content_types_provided(Req, State) ->
    {deliv_web_utils:content_type_json_map(to_json), Req, State}.

to_json(Req, State = #job{}) ->
    {chef_json:encode(jobs_job_json:to_json(State)), Req, State}.

delete_resource(Req, #job{status = running, runner_pid = RunnerPid} = Job) ->
    jobs_runner:cancel_job(RunnerPid),
    {true, Req, Job};
delete_resource(Req, #job{id = Id, deliv_ssh_job_pid = DelivSshJobPid} = Job) ->
    jobs_queue:remove_job(Id),
    deliv_ssh_job:job_canceled(DelivSshJobPid),
    {true, Req, Job}.

resource_exists(Req, State) ->
    case cowboy_req:binding(job_id, Req) of
        {undefined, Req1} -> {false, Req1, State};
        {JobId, Req1} ->
            case jobs_queue:find_job_by_id(JobId) of
                #job{} = Job -> {true, Req1, Job};
                _ -> {false, Req1, State}
            end
    end.

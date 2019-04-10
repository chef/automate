-module(jobs_handler_named_tests).

-include_lib("hoax/include/hoax.hrl").
-include("jobs_types.hrl").

-compile(export_all).

jobs_handler_named_test_() ->
    [
     eunit_sugar:fixture(?MODULE, "allowed_methods_"),
     eunit_sugar:fixture(?MODULE, "content_types_provided_"),
     hoax:fixture(?MODULE, "to_json_"),
     hoax:fixture(?MODULE, "resource_exists_"),
     hoax:fixture(?MODULE, "delete_resource_")
    ].

allowed_methods_is_get() ->
    ?assertEqual({[<<"GET">>, <<"DELETE">>], req, state}, jobs_handler_named:allowed_methods(req, state)).

content_types_provided_provides_json() ->
    Actual = jobs_handler_named:content_types_provided(req, state),
    Expected = {[{{<<"application">>,<<"json">>,'*'}, to_json}], req, state},

    ?assertEqual(Expected, Actual).

to_json_returns_ejson_when_provided_a_job() ->
    Job = #job{},

    hoax:expect(receive
                    jobs_job_json:to_json(Job) -> job_json;
                    chef_json:encode(job_json) -> encoded_json
                end),

    Actual = jobs_handler_named:to_json(req, Job),

    ?assertEqual({encoded_json, req, Job}, Actual),
    ?verifyAll.

resource_exists_returns_200_with_job_when_found_by_id() ->
    JobId = <<"id">>,
    Job = #job{id = JobId},

    hoax:expect(receive
                    cowboy_req:binding(job_id, req) -> {JobId, req1};
                    jobs_queue:find_job_by_id(JobId) -> Job
                end),

    Actual = jobs_handler_named:resource_exists(req, state),

    ?assertEqual({true, req1, Job}, Actual),
    ?verifyAll.

resource_exists_returns_404_when_no_job_id_on_request() ->
    hoax:expect(receive
                    cowboy_req:binding(job_id, req) -> {undefined, req1}
                end),

    Actual = jobs_handler_named:resource_exists(req, state),

    ?assertEqual({false, req1, state}, Actual),
    ?verifyAll.

resource_exists_returns_404_when_no_job_found_by_id() ->
    JobId = <<"id">>,

    hoax:expect(receive
                    cowboy_req:binding(job_id, req) -> {JobId, req1};
                    jobs_queue:find_job_by_id(JobId) -> not_found
                end),

    Actual = jobs_handler_named:resource_exists(req, state),

    ?assertEqual({false, req1, state}, Actual),
    ?verifyAll.

delete_resource_when_running_stops_runner_cancels_job_and_returns_true() ->
    Job = #job{status = running, runner_pid = runner_pid},

    hoax:expect(receive
                    jobs_runner:cancel_job(runner_pid) -> ok
                end),

    Actual = jobs_handler_named:delete_resource(req, Job),

    ?assertEqual({true, req, Job}, Actual),
    ?verifyAll.

delete_resource_when_pending_cancels_job_and_returns_true() ->
    Job = #job{id = some_id, deliv_ssh_job_pid = some_pid, status = pending},

    hoax:expect(receive
                    jobs_queue:remove_job(some_id) -> ok;
                    deliv_ssh_job:job_canceled(some_pid) -> ok
                end),

    Actual = jobs_handler_named:delete_resource(req, Job),

    ?assertEqual({true, req, Job}, Actual),
    ?verifyAll.

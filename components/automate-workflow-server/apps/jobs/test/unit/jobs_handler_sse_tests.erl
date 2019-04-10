-module(jobs_handler_sse_tests).

-include_lib("hoax/include/hoax.hrl").

-compile(export_all).

hoax_fixture_test_() ->
    [
     hoax:fixture(?MODULE, "init_"),
     hoax:fixture(?MODULE, "events_"),
     hoax:fixture(?MODULE, "format_event_"),
     hoax:fixture(?MODULE, "forbidden_callback_")
    ].

init_delegates_to_deliv_web_sse_init() ->
    hoax:expect(receive
                    deliv_web_sse:init(jobs_handler_sse, req, state) -> {ok, req1, state}
                end),

    Actual = jobs_handler_sse:init(transport, req, state),

    ?assertEqual({ok, req1, state}, Actual),
    ?verifyAll.

events_subscribes_to_update_jobs_queue_events() ->
    Actual = jobs_handler_sse:events(req, state),

    ?assertEqual({req, state, [update_job_queue]}, Actual).

format_event_formats_data() ->
    Jobs = [job1, job2],
    Ejson = {[{<<"key">>, <<"value">>}]},

    hoax:expect(receive
                    chef_utils:random_hex_string(16) -> "id";
                    jobs_job_json:to_json(Jobs) -> Ejson;
                    deliv_web_sse:format_event("id", <<"update_job_queue">>, {ejson, Ejson}) -> io_data
                 end),

    Actual = jobs_handler_sse:format_event(update_job_queue, Jobs, req1, state),

    ?assertEqual({keep_open, req1, state, io_data}, Actual),
    ?verifyAll.

forbidden_callback_returns_forbidden_none() ->
    ?assertEqual(forbidden_none, jobs_handler_sse:forbidden_callback()).

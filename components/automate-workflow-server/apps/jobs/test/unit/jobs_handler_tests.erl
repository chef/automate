-module(jobs_handler_tests).

-include_lib("hoax/include/hoax.hrl").

-compile(export_all).

hoax_fixture_test_() ->
    [
     eunit_sugar:fixture(?MODULE, "allowed_methods_"),
     eunit_sugar:fixture(?MODULE, "content_types_provided_"),
     hoax:fixture(?MODULE, "to_json_")
    ].

allowed_methods_is_get() ->
    ?assertEqual({[<<"GET">>, <<"DELETE">>], req, state}, jobs_handler_named:allowed_methods(req, state)).

content_types_provided_provides_json() ->
    Actual = jobs_handler_named:content_types_provided(req, state),
    Expected = {[{{<<"application">>,<<"json">>,'*'}, to_json}], req, state},

    ?assertEqual(Expected, Actual).

to_json_returns_list_of_queued_jobs() ->
    hoax:expect(receive
                    jobs_queue:fetch_state() -> [jobs];
                    jobs_job_json:to_json([jobs]) -> jobs_ejson;
                    chef_json:encode(jobs_ejson) -> jobs_json
                end),

    Actual = jobs_handler:to_json(req1, state),

    ?assertEqual({jobs_json, req1, state}, Actual),
    ?verifyAll.

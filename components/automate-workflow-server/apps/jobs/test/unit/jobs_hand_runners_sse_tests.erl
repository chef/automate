-module(jobs_hand_runners_sse_tests).

-include_lib("hoax/include/hoax.hrl").

-compile(export_all).

hoax_fixture_test_() ->
    [
     hoax:fixture(?MODULE, "init_"),
     hoax:fixture(?MODULE, "events_"),
     hoax:fixture(?MODULE, "format_event_"),
     eunit_sugar:fixture(?MODULE, "forbidden_callback_")
    ].

init_delegates_to_deliv_web_sse_init() ->
    hoax:expect(receive
                    deliv_web_sse:init(jobs_hand_runners_sse, req, state) -> {ok, req1, state}
                end),

    Actual = jobs_hand_runners_sse:init(transport, req, state),

    ?assertEqual({ok, req1, state}, Actual),
    ?verifyAll.

events_subscribes_to_update_jobs_runners_state_events() ->
    Actual = jobs_hand_runners_sse:events(req, state),

    ?assertEqual({req, state, [runners_state_updated]}, Actual).

format_event_formats_data() ->
    Runners = [runner1, runner2],
    EJson = ejson,

    hoax:expect(receive
                    chef_utils:random_hex_string(16) -> "id";
                    jobs_runner_json:to_json(Runners) -> EJson;
                    deliv_web_sse:format_event("id", <<"runners_state_updated">>, {ejson, EJson}) -> io_data
                end),

    Actual = jobs_hand_runners_sse:format_event(runners_state_updated, Runners, req1, state),

    ?assertEqual({keep_open, req1, state, io_data}, Actual),
    ?verifyAll.

forbidden_callback_returns_forbidden_none() ->
    ?assertEqual(forbidden_none, jobs_hand_runners_sse:forbidden_callback()).

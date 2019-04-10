-module(jobs_hand_runners_named_health_tests).

-include("jobs_types.hrl").
-include_lib("hoax/include/hoax.hrl").

-compile(export_all).

jobs_hand_runners_test_() ->
    [
     eunit_sugar:fixture(?MODULE, "allowed_methods_"),
     hoax:fixture(?MODULE, "process_post_")
    ].

allowed_methods_is_post() ->
    ?assertEqual({[<<"POST">>], req, state}, jobs_hand_runners_named_health:allowed_methods(req, state)).

process_post_triggers_health_check_and_returns_runner_with_state() ->
    State = #runner{},
    RunnerWithHealth = #runner{},
    hoax:expect(receive
                    jobs_runner_registry:health(State) -> RunnerWithHealth;
                    jobs_runner_json:to_json(RunnerWithHealth) -> runner_ejson;
                    chef_json:encode(runner_ejson) -> body;
                    cowboy_req:set_resp_body(body, req) -> req1
                end),

    Actual = jobs_hand_runners_named_health:process_post(req, State),

    ?assertEqual({true, req1, RunnerWithHealth}, Actual),
    ?verifyAll.

-module(jobs_hand_runners_tests).

-include("jobs_types.hrl").
-include_lib("hoax/include/hoax.hrl").

-compile(export_all).

jobs_hand_runners_test_() ->
    [
     hoax:fixture(?MODULE, "get_runners_"),
     hoax:fixture(?MODULE, "create_or_return_runner_")
    ].

get_runners_when_fetch_all_is_successful_returns_ejson_representation_of_existing_runners() ->
    Runners = possibly_empty_list_of_runners,
    EJSON = possibly_empty_array_of_runner_objects,
    hoax:expect(receive
                    jobs_runners_state:list() -> Runners;
                    jobs_runner_json:to_json(Runners) -> EJSON;
                    deliv_web_utils:content(EJSON, req, state) -> {body, req2, state}
                end),
    Actual = jobs_hand_runners:get_runners(req, state),
    ?assertEqual({body, req2, state}, Actual),
    ?verifyAll.

create_or_return_runner_returns_400_response_when_malformed_request() ->
    StateWithEnt = #handler{ent_name = my_ent},
    hoax:expect(receive
                    deliv_web_utils:parse_json_req(req, job_runner) -> {{error, why}, req1};
                    deliv_web_utils:error_response(400, bad_request, req1, StateWithEnt) -> {halt, req2, state}
                end),

    Actual = jobs_hand_runners:create_or_return_runner(req, StateWithEnt),

    ?assertEqual({halt, req2, state}, Actual),
    ?verifyAll.

create_or_return_runner_returns_500_when_runner_create_or_return_fails_with_unknown_error() ->
    Hostname = <<"somehost">>,
    Os = <<"linux">>,
    PlatformVersion = <<"14.04">>,
    PlatformFamily = <<"debian">>,
    Platform = <<"ubuntu">>,
    EJSON = {[
              {<<"hostname">>, Hostname},
              {<<"os">>, Os},
              {<<"platform_version">>, PlatformVersion},
              {<<"platform_family">>, PlatformFamily},
              {<<"platform">>, Platform}
             ]},
    StateWithEnt = #handler{ent_name = my_ent},
    hoax:expect(receive
                    deliv_web_utils:parse_json_req(req, job_runner) -> {EJSON, req1};
                    jobs_runner_registry:register(my_ent, Hostname, Os, PlatformFamily, Platform, PlatformVersion) -> {error, why};
                    deliv_web_utils:error_response(500, internal_error, req1, StateWithEnt) -> {halt, req2, state};
                    chef_log:error("Failed to create runner. Reason: ~p", [why]) -> ignored
                end),

    Actual = jobs_hand_runners:create_or_return_runner(req, StateWithEnt),

    ?assertEqual({halt, req2, state}, Actual),
    ?verifyAll.

create_or_return_runner_when_create_or_return_returns_runner_returns_no_error() ->
    Hostname = <<"somehost">>,
    Os = <<"linux">>,
    PlatformVersion = <<"14.04">>,
    PlatformFamily = <<"debian">>,
    Platform = <<"ubuntu">>,
    EJSON = {[
              {<<"hostname">>, Hostname},
              {<<"os">>, Os},
              {<<"platform_version">>, PlatformVersion},
              {<<"platform_family">>, PlatformFamily},
              {<<"platform">>, Platform}
             ]},
    StateWithEnt = #handler{ent_name = my_ent},
    hoax:expect(receive
                    deliv_web_utils:parse_json_req(req, job_runner) -> {EJSON, req1};
                    jobs_runner_registry:register(my_ent, Hostname, Os, PlatformFamily, Platform, PlatformVersion) -> {ok, runner};
                    jobs_runner_json:to_json(runner) -> ejson;
                    deliv_web_utils:content(ejson, req1, StateWithEnt) -> {body, req2, state};
                    cowboy_req:set_resp_body(body, req2) -> req3
                end),
    Actual = jobs_hand_runners:create_or_return_runner(req, StateWithEnt),
    ?assertEqual({true, req3, state}, Actual),
    ?verifyAll.

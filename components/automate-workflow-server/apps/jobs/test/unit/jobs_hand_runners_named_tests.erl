-module(jobs_hand_runners_named_tests).

-include("jobs_types.hrl").
-include_lib("hoax/include/hoax.hrl").

-compile(export_all).

jobs_hand_runners_test_() ->
    [
     eunit_sugar:fixture(?MODULE, "allowed_methods_"),
     hoax:fixture(?MODULE, "delete_resource_"),
     hoax:fixture(?MODULE, "to_json_")
    ].

allowed_methods_is_get_and_delete() ->
    ?assertEqual({[<<"GET">>, <<"DELETE">>], req, state}, jobs_hand_runners_named:allowed_methods(req, state)).

delete_resource_when_delete_returns_an_error_returns_false() ->
    State = #runner{enterprise_name = <<"enterprise">>, hostname = <<"host">>},
    hoax:expect(receive
                    jobs_runner_registry:delete(State) -> {error, reason};
                    chef_log:failed_call(jobs_runner_registry, delete, [State], reason) -> ignored
                end),

    Actual = jobs_hand_runners_named:delete_resource(req, State),

    ?assertEqual({false, req, State}, Actual),
    ?verifyAll.

delete_resource_when_delete_succeeds_returns_true_with_state() ->
    State = #runner{enterprise_name = <<"enterprise">>, hostname = <<"host">>},
    hoax:expect(receive
                    jobs_runner_registry:delete(State) -> ok
                end),

    Actual = jobs_hand_runners_named:delete_resource(req, State),

    ?assertEqual({true, req, State}, Actual),
    ?verifyAll.

to_json_converts_runner_record_and_returns_json_body() ->
    State = #runner{},
    hoax:expect(receive
                    jobs_runner_json:to_json(State) -> runner_ejson;
                    chef_json:encode(runner_ejson) -> body
                end),

    Actual = jobs_hand_runners_named:to_json(req, State),

    ?assertEqual({body, req, State}, Actual),
    ?verifyAll.

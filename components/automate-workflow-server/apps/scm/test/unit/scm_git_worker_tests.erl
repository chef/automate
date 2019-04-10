-module(scm_git_worker_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_coordinates.hrl").

-compile(export_all).

handle_cast_test_() ->
    hoax:fixture(?MODULE, "handle_cast_").

init_returns_initial_state_test() ->
    ?assertEqual({ok, {not_running, []}}, scm_git_worker:init([])).

handle_cast_when_force_push_and_initial_state_should_run_the_request() ->
    SourceBranch = <<"feature">>,
    DestBranch = <<"feature">>,
    Coords = #proj_coordinates{ent_name = <<"cd">>, org_name = <<"haha">>, proj_name = <<"project">>},
    Request = {force_push, [SourceBranch, DestBranch, Coords]},
    InitialState = {not_running, []},
    RunningState = {running, []},

    hoax:mock(scm_async_git_worker_sup,
              ?expect(force_push,
                      ?withArgs([SourceBranch, DestBranch, Coords]),
                      ?andReturn({ok, cool}))),

    ?assertEqual({noreply, RunningState}, scm_git_worker:handle_cast(Request, InitialState)),
    ?verifyAll.

handle_cast_when_force_push_and_running_state_should_enqueue_the_request() ->
    SourceBranch = <<"feature">>,
    DestBranch = <<"feature">>,
    Coords = #proj_coordinates{ent_name = <<"cd">>, org_name = <<"haha">>, proj_name = <<"project">>},
    Request = {force_push, [SourceBranch, DestBranch, Coords]},
    RunningState = {running, []},
    EnqueuedState = {running, [Request]},

    ?assertEqual({noreply, EnqueuedState}, scm_git_worker:handle_cast(Request, RunningState)),
    ?verifyAll.

handle_cast_when_force_push_and_enqueued_state_should_enqueue_the_request() ->
    SourceBranch = <<"feature">>,
    DestBranch = <<"feature">>,
    Coords = #proj_coordinates{ent_name = <<"cd">>, org_name = <<"haha">>, proj_name = <<"project">>},
    Request = {force_push, [SourceBranch, DestBranch, Coords]},
    Request1 = {force_push, [<<"feature1">>, DestBranch, Coords]},
    EnqueuedState = {running, [Request]},
    DeeperQueuedState = {running, [Request, Request1]},

    ?assertEqual({noreply, DeeperQueuedState}, scm_git_worker:handle_cast(Request1, EnqueuedState)),
    ?verifyAll.

handle_cast_when_request_finished_and_enqueued_state_should_run_the_enqueued_request() ->
    SourceBranch = <<"feature">>,
    DestBranch = <<"feature">>,
    Coords = #proj_coordinates{ent_name = <<"cd">>, org_name = <<"haha">>, proj_name = <<"project">>},
    Request = {force_push, [SourceBranch, DestBranch, Coords]},
    EnqueuedState = {running, [Request]},
    RunningState = {running, []},

    hoax:mock(scm_async_git_worker_sup,
              ?expect(force_push,
                      ?withArgs([SourceBranch, DestBranch, Coords]),
                      ?andReturn({ok, cool}))),

    ?assertEqual({noreply, RunningState}, scm_git_worker:handle_cast(request_finished, EnqueuedState)),
    ?verifyAll.

handle_cast_when_request_finished_and_deeper_enqueued_state_should_run_the_next_enqueued_request() ->
    SourceBranch = <<"feature">>,
    DestBranch = <<"feature">>,
    Coords = #proj_coordinates{ent_name = <<"cd">>, org_name = <<"haha">>, proj_name = <<"project">>},
    Request = {force_push, [SourceBranch, DestBranch, Coords]},
    Request1 = {force_push, [<<"feature1">>, DestBranch, Coords]},
    EnqueuedState = {running, [Request1]},
    DeeperEnqueuedState = {running, [Request, Request1]},

    hoax:mock(scm_async_git_worker_sup,
              ?expect(force_push,
                      ?withArgs([SourceBranch, DestBranch, Coords]),
                      ?andReturn({ok, cool}))),

    ?assertEqual({noreply, EnqueuedState}, scm_git_worker:handle_cast(request_finished, DeeperEnqueuedState)),
    ?verifyAll.

handle_cast_when_request_finished_and_running_state_should_go_to_initial_state() ->
    RunningState = {running, []},
    InitialState = {not_running, []},

    ?assertEqual({noreply, InitialState}, scm_git_worker:handle_cast(request_finished, RunningState)),
    ?verifyAll.

handle_cast_when_delete_branch_and_initial_state_runs_command() ->
    DestBranch = <<"feature">>,
    Coords = #proj_coordinates{ent_name = <<"cd">>, org_name = <<"haha">>, proj_name = <<"project">>},
    InitialState = {not_running, []},
    RunningState = {running, []},

    hoax:mock(scm_async_git_worker_sup,
              ?expect(delete_branch,
                      ?withArgs([DestBranch, Coords]),
                      ?andReturn({ok, cool}))),

    ?assertEqual({noreply, RunningState}, scm_git_worker:handle_cast({delete_branch, [DestBranch, Coords]}, InitialState)),
    ?verifyAll.

handle_info_returns_noreply_test() ->
    ?assertEqual({noreply, state}, scm_git_worker:handle_info(request, state)).

handle_call_returns_noreply_test() ->
    ?assertEqual({noreply, state}, scm_git_worker:handle_call(request, from, state)).

terminate_returns_ok_test() ->
    ?assertEqual(ok, scm_git_worker:terminate(reason, state)).

code_change_returns_ok_test() ->
    ?assertEqual({ok, state}, scm_git_worker:code_change(oldvsn, state, extra)).

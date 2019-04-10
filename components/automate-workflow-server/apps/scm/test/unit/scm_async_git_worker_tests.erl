-module(scm_async_git_worker_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_coordinates.hrl").

-compile(export_all).

start_link_test_() ->
    hoax:fixture(?MODULE, "start_link_").

init_test_() ->
    hoax:fixture(?MODULE, "init_").

handle_info_test_() ->
    hoax:fixture(?MODULE, "handle_info_").

handle_call_test_() ->
    hoax:fixture(?MODULE, "handle_call_").

handle_cast_test_() ->
    hoax:fixture(?MODULE, "handle_cast_").

terminate_test_() ->
    hoax:fixture(?MODULE, "terminate_").

code_change_() ->
    hoax:fixture(?MODULE, "code_change_").

handle_info_calls_force_push_and_returns_stop_normal_when_timeout_force_push_args() ->
    State = {force_push, [<<"SourceBranch">>, <<"DestBranch">>, #proj_coordinates{}]},

    hoax:mock(scm_git_client,
              ?expect(force_push,
                      ?withArgs([<<"SourceBranch">>, <<"DestBranch">>, #proj_coordinates{}]),
                      ?andReturn({ok, success}))),

    ?assertEqual({stop, normal, State}, scm_async_git_worker:handle_info(timeout, State)),
    ?verifyAll.

handle_info_calls_delete_branch_and_returns_stop_normal_when_timeout() ->
    State = {delete_branch, [<<"DestBranch">>, #proj_coordinates{}]},

    hoax:mock(scm_git_client,
              ?expect(delete_branch,
                      ?withArgs([<<"DestBranch">>, #proj_coordinates{}]),
                      ?andReturn({ok, success}))),

    ?assertEqual({stop, normal, State}, scm_async_git_worker:handle_info(timeout, State)),
    ?verifyAll.

handle_info_returns_noreply() ->
    ?assertEqual({noreply, state}, scm_async_git_worker:handle_info(info, state)).

handle_call_returns_noreply() ->
    ?assertEqual({noreply, state}, scm_async_git_worker:handle_call(request, from, state)).

handle_cast_returns_noreply() ->
    ?assertEqual({noreply, state}, scm_async_git_worker:handle_cast(request, state)).

terminate_calls_request_finished_and_returns_ok() ->
    hoax:mock(scm_git_worker,
              ?expect(request_finished,
                      ?withArgs([]),
                      ?andReturn(ignored))),
    ?assertEqual(ok, scm_async_git_worker:terminate(reason, state)),
    ?verifyAll.

code_change_returns_ok() ->
    ?assertEqual({ok, state}, scm_async_git_worker:code_change(oldvsn, state, extra)).

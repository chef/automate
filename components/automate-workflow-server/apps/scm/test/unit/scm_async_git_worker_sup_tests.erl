-module(scm_async_git_worker_sup_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_coordinates.hrl").

-compile(export_all).

start_link_test_() ->
    hoax:fixture(?MODULE, "start_link_").

force_push_test_() ->
    hoax:fixture(?MODULE, "force_push_").

delete_branch_test_() ->
    hoax:fixture(?MODULE, "delete_branch_").

init_test_() ->
    hoax:fixture(?MODULE, "init_").

start_link_calls_supervisor_start_link() ->
    hoax:mock(supervisor,
              ?expect(start_link,
                      ?withArgs([{local, scm_async_git_worker_sup}, scm_async_git_worker_sup, []]),
                      ?andReturn({ok, list_to_pid("<0.30.0>")}))),
    ?assertEqual({ok, list_to_pid("<0.30.0>")},
                 scm_async_git_worker_sup:start_link()),
    ?verifyAll.

force_push_calls_start_child() ->
    hoax:mock(supervisor,
              ?expect(start_child,
                      ?withArgs([scm_async_git_worker_sup,
                                 [force_push,
                                  [<<"source_branch">>, <<"dest_branch">>, #proj_coordinates{}]]]))),
    ?assertEqual(ok, scm_async_git_worker_sup:force_push(<<"source_branch">>,
                                                               <<"dest_branch">>,
                                                               #proj_coordinates{})),
    ?verifyAll.

delete_branch_calls_start_child() ->
    hoax:mock(supervisor,
              ?expect(start_child,
                      ?withArgs([scm_async_git_worker_sup,
                                 [delete_branch,
                                  [<<"branch">>, #proj_coordinates{}]]]))),
    ?assertEqual(ok, scm_async_git_worker_sup:delete_branch(<<"branch">>,
                                                                  #proj_coordinates{})),
    ?verifyAll.

init_returns_ok() ->
    ExpectedProcs = [{scm_async_git_worker,
                      {scm_async_git_worker, start_link, []},
                      temporary, 5000, worker, [scm_async_git_worker]}],
    Expected = {ok, {{simple_one_for_one, 5, 10}, ExpectedProcs}},
    ?assertEqual(Expected, scm_async_git_worker_sup:init([])).

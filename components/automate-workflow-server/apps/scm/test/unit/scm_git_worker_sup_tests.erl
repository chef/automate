-module(scm_git_worker_sup_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_coordinates.hrl").

-compile(export_all).

start_link_test_() ->
    hoax:fixture(?MODULE, "start_link_").

init_test_() ->
    hoax:fixture(?MODULE, "init_").

start_link_calls_supervisor_start_link() ->
    hoax:mock(supervisor,
              ?expect(start_link,
                      ?withArgs([{local, scm_git_worker_sup}, scm_git_worker_sup, []]),
                      ?andReturn({ok, list_to_pid("<0.30.0>")}))),
    ?assertEqual({ok, list_to_pid("<0.30.0>")},
                 scm_git_worker_sup:start_link()),
    ?verifyAll.

init_returns_ok() ->
    ExpectedProcs = [{scm_git_worker,
                      {scm_git_worker, start_link, []},
                      permanent, 5000, worker, [scm_git_worker]}],
    Expected = {ok, {{one_for_one, 5, 10}, ExpectedProcs}},
    ?assertEqual(Expected, scm_git_worker_sup:init([])).

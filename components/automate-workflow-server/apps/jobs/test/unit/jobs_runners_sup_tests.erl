-module(jobs_runners_sup_tests).

-include("jobs_types.hrl").
-include_lib("hoax/include/hoax.hrl").

-compile(export_all).

-define(CHILD(I, Type), {I, {I, start_link, []}, transient, 5000, Type, [I]}).

jobs_runners_sup_test_() ->
    [
     eunit_sugar:fixture(?MODULE, "init_"),
     hoax:fixture(?MODULE, "delete_"),
     hoax:fixture(?MODULE, "create_")
    ].

init_returns_correct_child_specs() ->
    {ok, {_RestartStrategy, ChildSpecs}} = jobs_runners_sup:init([]),
    ?assertEqual([
                  ?CHILD(jobs_runner, worker)
                 ],
                 ChildSpecs).

init_returns_correct_restart_strategy() ->
    {ok, {RestartStrategy, _ChildSpecs}} = jobs_runners_sup:init([]),
    ?assertEqual({simple_one_for_one, 5, 10}, RestartStrategy).

delete_accepts_a_runner_and_sends_kill_to_correct_pid() ->
    hoax:expect(receive
                    supervisor:terminate_child(jobs_runners_sup, some_pid) -> terminated
                end),

    Actual = jobs_runners_sup:delete(some_pid),
    ?assertEqual(terminated, Actual),
    ?verifyAll.

create_accepts_entname_and_hostname_and_start_child_when_no_existing_runner() ->
    hoax:expect(receive
                    supervisor:which_children(jobs_runners_sup) -> [];
                    supervisor:start_child(jobs_runners_sup, [jobs_runner,
                                                              [some_ent, some_host, os, platform_family,
                                                               platform, platform_version]]) -> {ok, pid}
                end),

    Actual = jobs_runners_sup:create(some_ent, some_host, os, platform_family, platform, platform_version),
    ?assertEqual({ok, pid}, Actual),
    ?verifyAll.

create_accepts_entname_and_hostname_and_returns_pid_when_existing_runners() ->
    hoax:expect(receive
                    supervisor:which_children(jobs_runners_sup) -> [{id, pid1, type, modules}, {id, pid2, type, modules}];
                    jobs_runner:fetch_ent_hostname(pid1) -> {<<"ent1">>, <<"host1">>};
                    jobs_runner:fetch_ent_hostname(pid2) -> {<<"ent2">>, <<"host2">>}
                end),

      Actual = jobs_runners_sup:create(<<"ent1">>, <<"host1">>, os, platform_family, platform, platform_version),
      ?assertEqual({ok, pid1}, Actual),
      ?verifyAll.

create_accepts_entname_and_hostname_and_creates_runner_when_no_existing() ->
    hoax:expect(receive
                    supervisor:which_children(jobs_runners_sup) -> [{id, pid1, type, modules}, {id, pid2, type, modules}];
                    jobs_runner:fetch_ent_hostname(pid1) -> {<<"ent1">>, <<"host1">>};
                    jobs_runner:fetch_ent_hostname(pid2) -> {<<"ent2">>, <<"host2">>};
                    supervisor:start_child(jobs_runners_sup, [jobs_runner, [<<"ent3">>, <<"host3">>, os, platform_family, platform, platform_version]]) -> {ok, pid3}
                end),

      Actual = jobs_runners_sup:create(<<"ent3">>, <<"host3">>, os, platform_family, platform, platform_version),
      ?assertEqual({ok, pid3}, Actual),
      ?verifyAll.

-module(jobs_sup_tests).

-include_lib("eunit/include/eunit.hrl").

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

init_returns_correct_child_specs_test() ->
    {ok, {_RestartStrategy, ChildSpecs}} = jobs_sup:init([]),
    ?assertEqual([
                  ?CHILD(jobs_queue, worker),
                  ?CHILD(jobs_runners_sup, supervisor),
                  ?CHILD(jobs_runners_state, worker),
                  ?CHILD(jobs_runner_registry, worker)
                 ],
                 ChildSpecs).

init_returns_correct_restart_strategy_test() ->
    {ok, {RestartStrategy, _ChildSpecs}} = jobs_sup:init([]),
    ?assertEqual({one_for_one, 5, 10}, RestartStrategy).

%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
-module(deliv_stage_event_listeners_sup).

-behaviour(supervisor).

-export([
         start_link/0,
         init/1
        ]).

-define(ONE_FOR_ONE_RESTART_STRATEGY,
        {one_for_one, 5, 10}).

-define(PERMANENT_CHILD_WORKER_SPEC (Module,Function,Args),
        {Module, {Module,Function,Args}, permanent, 5000, worker, [Module]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {?ONE_FOR_ONE_RESTART_STRATEGY,
          [
           ?PERMANENT_CHILD_WORKER_SPEC(deliv_changeset_lifecycle_listener,
                                         start_link, []),
           ?PERMANENT_CHILD_WORKER_SPEC(deliv_dashboard_stage_lifecycle_listener,
                                         start_link, [])
          ]}}.

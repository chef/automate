%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% Copyright 2015 Chef Software, Inc. All Rights Reserved.

-module(deliv_phase_tests).

-include_lib("hoax/include/hoax.hrl").
-include("../../src/deliv_phase.hrl").
-include("../../src/deliverance_types.hrl").

-compile([export_all]).

%%
%% Custom Assertions
%%

assert_JSON_equal({ExpectedJSON}, {ActualJSON}) ->
    lists:foreach(fun({Key, Value}) ->
      ?assertEqual(Value, proplists:get_value(Key, ActualJSON))
    end, ExpectedJSON),
    ?assertEqual(length(ExpectedJSON), length(ActualJSON)).


%%
%% Helpers
%%

desc_for_phase(Query, undefined) ->
    chef_utils:to_bin("proj - ~s", [Query]);
desc_for_phase(_, Description) ->
    chef_utils:to_bin("proj - ~s", [Description]).

%%
%% init
%%

deliv_phase_test_() ->
    [
     hoax:fixture(?MODULE, "init_"),
     hoax:fixture(?MODULE, "idle_")
    ].

setup_idle(SearchQuery, SearchDescription) ->
    Change = deliv_change:'#new'(),
    Phase = <<"unit">>,
    ScopingNames = [<<"ent">>, <<"org">>, <<"proj">>, <<"pipe">>],
    Patchset = deliv_patchset:'#new'(),
    StageRun = deliv_stage_run:fromlist([{id, 1}]),
    StageRunPid = list_to_pid("<0.0.0>"),
    PropList = [{stage_run_id, 1},
                {phase, Phase},
                {search_query, SearchQuery},
                {search_description, SearchDescription},
                {status, <<"idle">>},
                {run_log, <<"Waiting for a worker.">>},
                {description, desc_for_phase(SearchQuery, SearchDescription)}],

    PhaseRun = deliv_phase_run:fromlist(PropList),

    [Change, Patchset, StageRun, StageRunPid, PhaseRun, ScopingNames].

setup_init(SearchQuery, SearchDescription) ->
    ChangeId = <<"abc-123">>,
    Change = deliv_change:'#new'(),
    Phase = <<"unit">>,
    ScopingNames = [<<"ent">>, <<"org">>, <<"proj">>, <<"pipe">>],
    Patchset = deliv_patchset:'#new'(),
    StageRun = deliv_stage_run:fromlist([{id, 1}]),
    StageRunPid = list_to_pid("<0.0.0>"),
    PropList = [{stage_run_id, 1},
                {phase, Phase},
                {search_query, SearchQuery},
                {search_description, SearchDescription},
                {status, <<"idle">>},
                {run_log, <<"Waiting for a worker.">>},
                {description, desc_for_phase(SearchQuery, SearchDescription)}],

    PhaseRun = deliv_phase_run:fromlist(PropList),
    PhaseRunId = 1,

    hoax:expect(receive
                    deliv_change:getval(id, Change) -> ChangeId;
                    deliv_change:scoping_names(ChangeId) -> ScopingNames;
                    deliv_phase_run:insert(PropList) -> [PhaseRun];
                    deliv_phase_run:getval(id, PhaseRun) -> PhaseRunId;
                    deliv_event:subscribe({phase_run, PhaseRunId}) -> ignore;
                    deliv_event:publish(phase_state_event, ?any) -> ignore
                end),

    [Change, Phase, Patchset, StageRun, StageRunPid, PhaseRun, ScopingNames].

init_returns_idle_when_phase_is_not_skipped() ->
    Skip = false,
    SearchQuery = <<"name:builder*">>,
    SearchDescription = undefined,
    [Change, Phase, Patchset, StageRun, StageRunPid, PhaseRun, ScopingNames] = setup_init(SearchQuery, SearchDescription),
    InitState = #deliv_phase{
                   change = Change,
                   patchset = Patchset,
                   stage_run = StageRun,
                   stage_run_pid = StageRunPid,
                   phase_run = PhaseRun,
                   scoping_names = ScopingNames,
                   job_dispatch_version = some_version,
                   criteria = none,
                   timeout = 5000
                  },

    Actual = deliv_phase:init({{Change, Patchset, StageRun, StageRunPid, Phase, SearchQuery, SearchDescription, some_version, none, 5000}, Skip}),
    ?assertEqual({ok, idle, InitState}, Actual),
    ?verifyAll.

init_returns_skip_when_phase_is_skipped() ->
    Skip = true,
    SearchQuery = <<"name:builder*">>,
    SearchDescription = <<"windows builders">>,
    [Change, Phase, Patchset, StageRun, StageRunPid, PhaseRun, ScopingNames] = setup_init(SearchQuery, SearchDescription),
    InitState = #deliv_phase{
                   change = Change,
                   patchset = Patchset,
                   stage_run = StageRun,
                   stage_run_pid = StageRunPid,
                   phase_run = PhaseRun,
                   scoping_names = ScopingNames,
                   job_dispatch_version = some_version,
                   criteria = none,
                   timeout = 5000
                  },

    Actual = deliv_phase:init({{Change, Patchset, StageRun, StageRunPid, Phase, SearchQuery, SearchDescription, some_version, none, 5000}, Skip}),
    ?assertEqual({ok, skip, InitState}, Actual),
    ?verifyAll.

idle_returns_next_state_waiting_worker_with_phase_job_pid_for_v1_job_dispatch() ->
    SearchQuery = ignored,
    SearchDescription = ignored,
    [Change, Patchset, StageRun, StageRunPid, PhaseRun, ScopingNames] = setup_idle(SearchQuery, SearchDescription),
    InitState = #deliv_phase{
                   change = Change,
                   patchset = Patchset,
                   stage_run = StageRun,
                   stage_run_pid = StageRunPid,
                   phase_run = PhaseRun,
                   scoping_names = ScopingNames,
                   job_dispatch_version = ?JOB_DISPATCH_V1,
                   criteria = none,
                   timeout = 5000
                  },
    StateWithPid = InitState#deliv_phase{ phase_job_pid = some_pid },

    hoax:expect(receive
                    deliv_event:publish({{phase, waiting_for_worker}, unit}, ?any) -> ignore;
                    deliv_event:publish(phase_state_event, {waiting_worker, StateWithPid}) -> ignore2;
                    deliv_phase_job:dispatch(InitState) -> {ok, some_pid}
                end),

    Actual = deliv_phase:idle(start, InitState),

    ?assertEqual({next_state, waiting_worker, StateWithPid}, Actual),
    ?verifyAll.

%%
%% Event Subscriptions
%%

sub_fixture_test_() ->
    hoax:fixture(?MODULE, subscribe).

subscribe_all_events() ->
    hoax:mock(deliv_phase_event,
              ?expect(subscribe,
                      ?withArgs([]))),

    deliv_phase:subscribe_phase_events(),
    ?verifyAll.

subscribe_specific_action() ->
    Action = finished,
    hoax:mock(deliv_phase_event,
              ?expect(subscribe,
                      ?withArgs([all, Action]))),

    deliv_phase:subscribe_phase_events(all, Action),
    ?verifyAll.

subscribe_specific_phase() ->
    Phase = unit,
    hoax:mock(deliv_phase_event,
              ?expect(subscribe,
                      ?withArgs([Phase, all]))),

    deliv_phase:subscribe_phase_events(Phase, all),
    ?verifyAll.

subscribe_specific_action_and_phase() ->
    Action = finished,
    Phase = unit,
    hoax:mock(deliv_phase_event,
              ?expect(subscribe,
                      ?withArgs([Phase, Action]))),

    deliv_phase:subscribe_phase_events(Phase, Action),
    ?verifyAll.

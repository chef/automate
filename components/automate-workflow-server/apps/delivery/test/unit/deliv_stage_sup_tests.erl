-module(deliv_stage_sup_tests).

-include_lib("hoax/include/hoax.hrl").

-compile([export_all]).

deliv_stage_sup_fixture_test_() ->
    hoax:fixture(?MODULE, start_link_).

start_link_starts_no_stage_runs_if_no_stage_runs_need_restart() ->
    Pid = list_to_pid("<0.1.0>"),
    hoax:expect(receive
                    supervisor:start_link({local, deliv_stage_sup}, deliv_stage_sup, []) -> {ok, Pid};
                    chef_log:info("Looking for changes to re-start.") -> ignored;
                    deliv_stage_run:get_stage_runs_to_restart() -> [];
                    chef_log:info("Done re-starting stages.") -> ignored
                end),
    ?assertEqual({ok, Pid}, deliv_stage_sup:start_link()),
    ?verifyAll.

start_link_restarts_stage_runs_needing_restart() ->
    Pid = list_to_pid("<0.1.0>"),
    ChangeId = <<"62794f41-7a0a-40fc-ae15-13b4aae99e8d">>,
    StageRunId = 1,
    StageRun = deliv_stage_run:'#new'(),
    StageAtom = verify,
    PhaseRun = deliv_phase_run:'#new'(),
    hoax:expect(receive
                    supervisor:start_link({local, deliv_stage_sup}, deliv_stage_sup, []) -> {ok, Pid};
                    deliv_stage_run:get_stage_runs_to_restart() -> [StageRun];
                    deliv_stage_run:getval(change_id, StageRun) -> ChangeId;
                    deliv_stage_run:getval(stage, StageRun) -> <<"verify">>;
                    chef_log:info("Looking for changes to re-start.") -> ignored;
                    deliv_change:trigger_stage(StageAtom, ChangeId) -> ok;
                    chef_log:info("Done re-starting stages.") -> ignored;
                    deliv_stage_run:getval(id, StageRun) -> StageRunId;
                    deliv_phase_run:fetch_by_stage_run_id(StageRunId) -> [PhaseRun];
                    deliv_phase_run:setvals([{status, <<"failed">>}], PhaseRun) -> PhaseRun;
                    deliv_phase_run:update(PhaseRun) -> ok;
                    deliv_stage_run:update([{status, <<"failed">>}], StageRun) -> StageRunId
                end),
    ?assertEqual({ok, Pid}, deliv_stage_sup:start_link()),
    ?verifyAll.

start_link_on_error_updates_stage_run() ->
    Pid = list_to_pid("<0.1.0>"),
    ChangeId = chef_utils:to_bin("62794f41-7a0a-40fc-ae15-13b4aae99e8d"),
    StageRunId = 1,
    StageRun = deliv_stage_run:'#new'(),
    PhaseRun = deliv_phase_run:'#new'(),
    StageAtom = verify,
    Error = patchset_not_found,
    hoax:expect(receive
                    supervisor:start_link({local, deliv_stage_sup}, deliv_stage_sup, []) -> {ok, Pid};
                    deliv_stage_run:get_stage_runs_to_restart() -> [StageRun];
                    deliv_stage_run:getval(change_id, StageRun) -> ChangeId;
                    deliv_stage_run:getval(stage, StageRun) -> <<"verify">>;
                    deliv_change:trigger_stage(StageAtom, ChangeId) -> {error, Error};
                    chef_log:info("Looking for changes to re-start.") -> ignored;
                    chef_log:info("Could not restart stage '~s' for change '~s': ~p", [StageAtom, ChangeId, Error]) -> ignored;
                    chef_log:info("Done re-starting stages.") -> ignored;
                    deliv_stage_run:getval(id, StageRun) -> StageRunId;
                    deliv_phase_run:fetch_by_stage_run_id(StageRunId) -> [PhaseRun];
                    deliv_phase_run:setvals([{status, <<"failed">>}], PhaseRun) -> PhaseRun;
                    deliv_phase_run:update(PhaseRun) -> ok;
                    deliv_stage_run:update([{status, <<"failed">>}], StageRun) -> StageRunId
                end),
    ?assertEqual({ok, Pid}, deliv_stage_sup:start_link()),
    ?verifyAll.

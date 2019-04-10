%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Adam Jacob <adam@opscode.com>
%% Copyright 2013 Opscode, Inc. All Rights Reserved.
%%

%% @doc A common supervisor for all deliv_stage workers
-module(deliv_stage_sup).
-behaviour(supervisor).

-export([init/1,
         start_link/0,
         start_stage/1]).

-include("deliv_types.hrl").

-spec start_stage(deliv_stage()) -> {ok, pid()}.
start_stage(StateData) ->
    supervisor:start_child(?MODULE, [StateData]).

-spec start_link() -> {ok, pid()}.
start_link() ->
    case supervisor:start_link({local, ?MODULE}, ?MODULE, []) of
        {ok, _Pid} = Success ->
            %% restarting stages, unless the `restart_stages_on_boot'
            %% app env setting's been set to `false'
            case delivery_app:get_env(restart_stages_on_boot, true) of
                true -> restart_stages();
                false -> ok
            end,
            Success;
        Error ->
            Error
    end.

init([]) ->
    {ok, {{simple_one_for_one, 10, 10},
          [{deliv_stage,
            {deliv_stage, start_link, []},
            transient,
            infinity,
            worker,
            [deliv_stage]}]}}.

%% @doc This function is used to restart any stages that are in running state
%% on restart. It finds all the running stage runs and sets them along with
%% their phase runs to failed/finished. Then it checks if the change is in a
%% state that it should be re-started. This means checking where it is in the
%% pipeline. If it is in verify it will always be restarted, build and
%% acceptance it must be the latest for both the stage and pipline, for union,
%% rehearsal, delivered it must be the latest for the stage.
%% Note that this feature can be deactivated by setting the
%% `restart_stages_on_boot
-spec restart_stages() -> ok.
restart_stages() ->
    chef_log:info("Looking for changes to re-start."),
    case deliv_stage_run:get_stage_runs_to_restart() of
        RunningStages when is_list(RunningStages) ->
            handle_restart_stages(RunningStages);
        {error, Error} ->
            %% Ignore Error and continue.
            chef_log:info("Could not load stages to re-start: ~p", [Error])
    end,
    chef_log:info("Done re-starting stages."),
    ok.

handle_restart_stages(StageRuns) ->
    lists:foreach(fun(StageRun) ->
                      ChangeId = deliv_stage_run:getval(change_id, StageRun),
                      Stage = deliv_stage_run:getval(stage, StageRun),
                      StageAtom = erlang:binary_to_existing_atom(Stage, utf8),

                      %% Trigger stage_run
                      case deliv_change:trigger_stage(StageAtom, ChangeId) of
                          {error, Error} ->
                              chef_log:info("Could not restart stage '~s'"
                                             " for change '~s': ~p",
                                             [StageAtom, ChangeId, Error]);
                           _ -> ok
                      end,

                      %% Update stage_run regardless
                      update_stage_run(StageRun),
                      ok
                  end,
                  StageRuns).

update_stage_run(StageRun) ->
  StageRunId = deliv_stage_run:getval(id, StageRun),

  case deliv_phase_run:fetch_by_stage_run_id(StageRunId) of
      PhaseRuns when is_list(PhaseRuns) ->
          update_phase_runs(PhaseRuns);
      {error, Error} ->
          chef_log:info("Could not update stage '~s': ~p.", [StageRunId, Error])
  end,

  deliv_stage_run:update([{status, <<"failed">>}], StageRun).

update_phase_runs(PhaseRuns) ->
    lists:foreach(fun(PhaseRun) ->
                      PR2 = deliv_phase_run:setvals([{status, <<"failed">>}], PhaseRun),
                      deliv_phase_run:update(PR2),
                      ok
                  end,
                  PhaseRuns).

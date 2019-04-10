%%
%% Making changes to deliv_stage is daunting because it is already huge and
%% we did not know whether the test coverage was rigorous. So when we introduced
%% support for the v2 build runners we copied the method signature and put
%% the implementation into a new file. This keeps deliv_stage from growing
%% and allows us to get complete test coverage for this module without having
%% to interfere with existing deliv_stage tests.
%%

-module(deliv_stage_project_v2).

-include("deliv_stage.hrl").

%% Public API
-export([
         create_phases_from_phase_group/2
        ]).

-spec create_phases_from_phase_group([atom()], deliv_stage()) -> deliv_stage().
create_phases_from_phase_group(PhaseGroup, DelivStage) ->
    start_phase_group([chef_utils:to_bin(Phase) || Phase <- PhaseGroup], DelivStage#deliv_stage{running_phases = []}).

-spec start_phase_group([binary()], deliv_stage()) -> deliv_stage().
start_phase_group([Phase|Phases], DelivStage = #deliv_stage{project_config=ProjectConfig}) ->
    Criteria = deliv_proj_config:get_phase_filters_from_config(ProjectConfig, Phase),
    NewDelivStage = start_phases_for_criteria(Phase, Criteria, DelivStage),
    start_phase_group(Phases, NewDelivStage);
start_phase_group([], DelivStage) ->
    DelivStage.

-spec start_phases_for_criteria(binary(), [deliv_ssh_job_criteria()], deliv_stage()) -> deliv_stage().
start_phases_for_criteria(Phase, [Criterion | Tail], DelivStage = #deliv_stage{running_phases=RunningPhases}) ->
    NewRunningPhases = case start_phase(Phase, Criterion, DelivStage) of
        {ok, Pid} -> [Pid|RunningPhases];
        {error, normal} -> RunningPhases
    end,
    start_phases_for_criteria(Phase, Tail, DelivStage#deliv_stage{running_phases=NewRunningPhases});
start_phases_for_criteria(_Phase, [], DelivStage) ->
    DelivStage.

-spec start_phase(binary(), deliv_ssh_job_criteria(), deliv_stage()) -> {ok, pid()} | {error, term()}.
start_phase(Phase, Criterion, #deliv_stage{change=Change,
                                           patchset=Patchset,
                                           stage_run=StageRun,
                                           skip_phase_groups=SkippedPhases,
                                           project_config=ProjectConfig}) ->
    Skip = lists:member(Phase, SkippedPhases),
    Timeout = deliv_proj_config:get_timeout_for_phase(ProjectConfig, Phase),
    deliv_phase:start_link(Change, Patchset, StageRun, self(), Phase, undefined, deliv_proj_config:criteria_to_json(Criterion), Skip, ?JOB_DISPATCH_V2, Criterion, Timeout).

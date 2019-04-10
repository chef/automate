%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Adam Jacob <adam@opscode.com>
%% @author Tom Duffield <tom@chef.io>
%% Copyright 2013-2015 Chef Software, Inc. All Rights Reserved.
%%

%% @doc Common FSM for all stages
-module(deliv_stage).
-behaviour(gen_fsm).

-include("deliv_stage.hrl").
-include("deliv_events.hrl").

%% Generic behaviour callbacks
-export([init/1, handle_event/3, terminate/3,
         code_change/4, handle_info/3, handle_sync_event/4]).

%% FSM States
-export([idle/2, running/2]).

%% Delivery API
-export([
         get_change/1,
         get_scoping_names/1,
         get_stage_name/1,
         run/1,
         start_link/1,
         subscribe_stage_events/0,
         trigger/2
        ]).

%% Used by deliv_phase to update the FSM
-export([phase_finished/3]).

%% Effectively private, but used for CT tests
-export([phase_groups_for/1,
         next_stage/1]).

%%
%%% API funs

-spec start_link(#deliv_stage{}) -> {ok, pid()}.
start_link(#deliv_stage{} = State) ->
    gen_fsm:start_link(?MODULE, State, []).

%% @doc Triggers a stage. Returns `ok' if the stage has successfully been queued
-spec trigger(deliv_stage_name(), d_change())
        -> ok | {error, invalid_config | _Other}.
trigger(StageName, Change) ->
    case initial_state(StageName, Change) of
        {ok, #deliv_stage{} = DelivStage} ->
            queue_stage(DelivStage);
        {error, _Why} = Error ->
            Error
    end.

%% @doc Meant to be called by deliv_phase workers to let the FSM
%% know when they're done
-spec phase_finished(pid(), pid(), failed | passed | skipped) -> ok.
phase_finished(StagePid, PhasePid, Status) when Status =:= failed
                                                orelse Status =:= passed
                                                orelse Status =:= skipped ->
    gen_fsm:send_event(StagePid, {phase_finished, PhasePid, Status}).

%% @doc Called to start the phase worker
-spec run(pid()) -> ok.
run(Pid) ->
    gen_fsm:send_event(Pid, start).

%% @doc Returns the change record from the internal state
-spec get_change(#deliv_stage{}) -> d_change().
get_change(#deliv_stage{change=Change}) -> Change.

%% @doc Returns the stage name from the internal state
-spec get_stage_name(#deliv_stage{}) -> deliv_stage_name().
get_stage_name(#deliv_stage{stage_name=StageName}) -> StageName.

%% @doc Returns the scoping names from the internal state.
%% `[EntName, OrgName, ProjName, PipeName]'
-spec get_scoping_names(#deliv_stage{}) -> [binary()].
get_scoping_names(#deliv_stage{scoping_names=ScopingNames}) -> ScopingNames.

%%%
%%% Internal States Diagram
%%%
%%
%% state             event                  next
%% -------------------------------------------------------
%% idle              start                 running
%%                     - if there's a next phase group,
%%                       create deliv_phase workers for it,
%%                       and call run on 'em
%%                     - otherwise, terminate
%% running         phase_finished        running | idle
%%                     - update the job's status
%%                     - all workers' status reported?
%%                       - if all successful, next state idle
%%                       - if one or more failed, terminate
%%                     - if not all reported, next state running

-spec idle(start, #deliv_stage{})
        -> {next_state, running, #deliv_stage{}}
           | {stop, normal, #deliv_stage{}}.
idle(start, State) ->
    Result = start(State),
    publish_build_event(State),
    Result.

%% @private
start(#deliv_stage{change=Change,
                   stage_run=StageRun,
                   stage_name=StageName,
                   running_phases=[],
                   failed=failed,
                   scoping_names=ScopingNames} = State) ->
    %% some stages have failed, we don't go any further
    log(info, State, "Stage failed, some phases unsuccessful"),
    ChangeId = deliv_change:getval(id, Change),
    notify_failure(StageName, ChangeId, ScopingNames),
    {ok, NewStageRun} = deliv_stage_run:finish(StageRun, failed),
    publish_stage_finished(State, <<"failed">>),
    {stop, normal, State#deliv_stage{stage_run=NewStageRun}};
start(#deliv_stage{next_phase_groups=[],
                   running_phases=[],
                   consumer_change_ids=[_|_],
                   child_stage_pids = undefined,
                   stage_name=union,
                   failed=not_failed_yet} = State) ->
    %% We've successfully finished the main phases for union and we have at least
    %% one consumer we need to test against.
    NestedStagePids = create_child_stages(State),
    UpdatedState = State#deliv_stage{child_stage_pids = NestedStagePids},
    {next_state, running, UpdatedState};
start(#deliv_stage{change=Change,
                   stage_run=StageRun,
                   stage_name=StageName,
                   next_phase_groups=[],
                   running_phases=[],
                   failed=not_failed_yet} = State) ->
    %% no phase groups (or consumers) left, and we never failed:
    %% smells like victory, my friend!
    log(info, State, "Stage passed, all phases successful"),
    {ok, NewStageRun} = deliv_stage_run:finish(StageRun, passed),
    %% proceed to the next stage, if any
    trigger_next_stage(StageName, Change),
    publish_stage_finished(State, <<"passed">>),
    {stop, normal, State#deliv_stage{stage_run=NewStageRun}};
start(#deliv_stage{running_phases=[],
                   failed=not_failed_yet} = State) ->
    %% not failed yet, and some phase groups left, let's carry on
    State2 = create_phases(State),
    State3 = run_phases(State2),
    {next_state, running, State3}.

-spec running({child_event(), pid(), #deliv_stage{}}, #deliv_stage{})
        -> {next_state, running, #deliv_stage{}}
           | {stop, normal, #deliv_stage{}}.
running({phase_finished, PhasePid, PhaseStatus},
        #deliv_stage{running_phases=RunningPhases} = State) ->
    NewRunningPhases = lists:delete(PhasePid, RunningPhases),
    NewState = State#deliv_stage{running_phases = NewRunningPhases},
    handle_phase_finished(PhaseStatus, NewState);
running({child_stage_finished, ChildPid, PipelineIdAndStatus},
          #deliv_stage{child_stage_pids = ChildrenPids} = State) ->
    NewChildrenPids = lists:delete(ChildPid, ChildrenPids),
    NewState = State#deliv_stage{child_stage_pids = NewChildrenPids},
    handle_child_stage_finished(PipelineIdAndStatus, NewState).


-spec init(#deliv_stage{}) -> {ok, idle, #deliv_stage{}}.
init(#deliv_stage{} = State) ->
    publish_stage_started(State),
    {ok, idle, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.


%%% End of FSM funs

%%% Internals

%% @private
handle_child_stage_finished({PipelineId, failed}, #deliv_stage{stage_run=StageRun, failed_pipelines=FailedPipelineIds} = State) ->
    {ok, NewStageRun} = deliv_stage_run:update([{status, <<"failed">>}], StageRun),
    do_finish_child_stage(State#deliv_stage{stage_run = NewStageRun, failed = failed, failed_pipelines = [PipelineId | FailedPipelineIds]});
handle_child_stage_finished({PipelineId, not_failed_yet}, #deliv_stage{passed_pipelines=PassedPipelineIds} = State) ->
    do_finish_child_stage(State#deliv_stage{passed_pipelines=[PipelineId | PassedPipelineIds]}).

%% @private
do_finish_child_stage(#deliv_stage{child_stage_pids=[]} = State) ->
    idle(start, State);
do_finish_child_stage(#deliv_stage{child_stage_pids=[_|_]} = State) ->
    publish_build_event(State),
    {next_state, running, State}.

%% @private
handle_phase_finished(failed, #deliv_stage{stage_run=StageRun} = State) ->
    {ok, NewStageRun} = deliv_stage_run:update([{status, <<"failed">>}], StageRun),
    do_finish_phase(State#deliv_stage{stage_run = NewStageRun, failed = failed});
handle_phase_finished(_Status, State) ->
    do_finish_phase(State).

%% @private
do_finish_phase(#deliv_stage{running_phases=[],
                             next_phase_groups=[],
                             parent_stage_pid=Pid,
                             failed=Failed,
                             change=Change} = State) when erlang:is_pid(Pid) ->
    PipelineId = deliv_change:getval(pipeline_id, Change),
    gen_fsm:send_event(Pid, {child_stage_finished, self(), {PipelineId, Failed}}),
    {stop, normal, State};
do_finish_phase(#deliv_stage{running_phases=[],
                             parent_stage_pid=Pid,
                             failed=failed,
                             change=Change} = State) when erlang:is_pid(Pid) ->
    PipelineId = deliv_change:getval(pipeline_id, Change),
    gen_fsm:send_event(Pid, {child_stage_finished, self(), {PipelineId, failed}}),
    {stop, normal, State};
do_finish_phase(#deliv_stage{running_phases=[_|_]} = State) ->
    publish_build_event(State),
    {next_state, running, State};
% When a parent is entirely done with every phase with no failures,
% add it to passed_pipelines.
do_finish_phase(#deliv_stage{running_phases=[],
                             next_phase_groups=[],
                             parent_stage_pid=undefined,
                             failed=not_failed_yet,
                             change=Change} = State) ->
    PipeId = deliv_change:getval(pipeline_id, Change),
    idle(start, State#deliv_stage{passed_pipelines=[PipeId]});
% When a parent fails a phase, add it to failed_pipelines.
do_finish_phase(#deliv_stage{running_phases=[],
                             parent_stage_pid=undefined,
                             failed=failed,
                             change=Change} = State) ->
    PipeId = deliv_change:getval(pipeline_id, Change),
    idle(start, State#deliv_stage{failed_pipelines=[PipeId]});
do_finish_phase(State) ->
    idle(start, State).

%% @private
%% For each consumer change ID in our state, trigger a child stage run.
create_child_stages(#deliv_stage{consumer_change_ids=ChildChangeIds} = ParentState) ->
    create_child_stages(ChildChangeIds, [], ParentState).

%% @private
%% Child stage run launch accumulator
create_child_stages([], ChildPids, _) ->
    ChildPids;
create_child_stages([ChangeId | RemChangeIds], Acc, ParentState) ->
    case start_stage_for_change(deliv_change:fetch_by_id(ChangeId), ParentState) of
        {ok, Pid} ->
            deliv_stage:run(Pid),
            create_child_stages(RemChangeIds, [Pid | Acc], ParentState);
        {error, Why} ->
            chef_log:failed_call(?MODULE, start_stage_for_change, [ChangeId, ParentState], Why),
            create_child_stages(RemChangeIds, Acc, ParentState)
    end.

%% @private
%% Given a Change, trigger a child stage run nested under the current stage run
start_stage_for_change({ok, Change}, #deliv_stage{stage_run = StageRun}) ->
    case initial_state_for_change(union, Change, []) of
        {ok, State} ->
            Stage = State#deliv_stage{stage_run = StageRun,
                                      parent_stage_pid = self()},
            deliv_stage_sup:start_stage(Stage);
        {error, _} = Error ->
            Error
    end;
start_stage_for_change({error, _} = Error, _) -> Error.

%% @private
%% @doc When in union, we need to find the consumer change IDs. Otherwise, just
%% set those change_ids to an empty list.
-spec initial_state(deliv_stage_name(), d_change())
        -> {ok, #deliv_stage{}} | {error, invalid_config | _Other}.
initial_state(union, Change) ->
    PipelineId = deliv_change:getval(pipeline_id, Change),
    ChangeIds = deliv_changeset:consumer_change_ids_for_pipeline(PipelineId),
    do_initial_state(union, Change, ChangeIds);
initial_state(StageName, Change) ->
    do_initial_state(StageName, Change, {ok, []}).

%% @private
%% Create the initial deliv_stage state
do_initial_state(StageName, Change, {error, _Why}) ->
    do_initial_state(StageName, Change, {ok, []});
do_initial_state(StageName, Change, {ok, ConsumerChangeIds}) ->
    case initial_state_for_change(StageName, Change, ConsumerChangeIds) of
        {ok, State} ->
            ChangeId = deliv_change:getval(id, Change),
            [StageRun] = deliv_stage_run:insert([{change_id, ChangeId},
                                                 {stage, chef_utils:to_bin(StageName)},
                                                 {status, <<"idle">>}]),
            {ok, State#deliv_stage{stage_run = StageRun}};
        {error, _} = Error ->
            Error
    end.

%% @private
%% Create a deliv_stage record for a given StageName and Change
initial_state_for_change(StageName, Change, ConsumerChangeIds) ->
    ChangeId = deliv_change:getval(id, Change),
    case deliv_patchset:latest_patchset_for_change(ChangeId) of
        {ok, Patchset} ->
            initial_state_for_change(StageName, Change, ConsumerChangeIds, ChangeId, Patchset);
        {error, _} = Error ->
            Error
    end.
initial_state_for_change(StageName, Change, ConsumerChangeIds, ChangeId, Patchset) ->
    Scope = deliv_scopes:from_change_id(ChangeId),

    %% load the config
    [ScmMod, ScopingNames] = deliv_scopes:'#get'([scm_module, scoping_names], Scope),
    case ScmMod:load_config_for_patchset(Patchset, Scope) of
        {ok, ProjectConfig} ->
            {ok, #deliv_stage{change=Change,
                              patchset=Patchset,
                              stage_name=StageName,
                              next_phase_groups=phase_groups_for(StageName),
                              skip_phase_groups=get_phases_to_skip_from(ProjectConfig),
                              running_phases=[],
                              project_config=ProjectConfig,
                              scoping_names=ScopingNames,
                              scope=Scope,
                              consumer_change_ids=ConsumerChangeIds}};
        {error, _} = Error ->
            Error
    end.

%% @private
%% @doc For a given StageName, grab the StageData for that stage and return the
%% list of phases that should be triggered.
-spec phase_groups_for(deliv_stage_name()) -> [phase_group()].
phase_groups_for(StageName) ->
    {StagePhases, _NextStage} = stage_data_for(StageName),
    StagePhases.

%% @private
%% @doc Pull the StageData from the STAGES_DATA_FILENAME. This stage data will
%% include the list of phases associated with that stage as well as the
%% subsequent stage to trigger on success.
-spec stage_data_for(deliv_stage_name()) -> tuple().
stage_data_for(StageName) ->
    {StageName, StageData} = lists:keyfind(StageName, 1, deliv_stage_data:stages()),
    StageData.

%% @private
%% @doc Look in the project config to determine if there are any phases we
%% should skip. If there are, return a list of the phase names. If there are
%% none, return an empty list.
- spec get_phases_to_skip_from(tuple()) -> [binary()].
get_phases_to_skip_from(ProjectConfig) ->
    deliv_proj_config:get_phases_to_skip(ProjectConfig).

%% @private
%% @doc For the given stage, start the process of creating the deliv_phase
%% workers for each phase in the phase group.
-spec create_phases(#deliv_stage{}) -> #deliv_stage{}.
create_phases(#deliv_stage{next_phase_groups=[PhaseItem | RestPhaseItems], project_config=ProjectConfig} = State) ->
    NewState = State#deliv_stage{next_phase_groups=RestPhaseItems},
    create_phases_from_phase_group(deliv_proj_config:job_dispatch_version(ProjectConfig), PhaseItem, NewState).

%% @private
%% @doc For each phase in the phase group specified in the application config,
%% launch a deliv_phase worker.
-spec create_phases_from_phase_group(binary(), phase_group(), #deliv_stage{})
        -> #deliv_stage{}.
create_phases_from_phase_group(?JOB_DISPATCH_V1, SinglePhase, State) when is_atom(SinglePhase) ->
    create_phases_from_phase_group(?JOB_DISPATCH_V1, [SinglePhase], State);
create_phases_from_phase_group(?JOB_DISPATCH_V1, PhaseItem, #deliv_stage{change=Change,
                                                       patchset=Patchset,
                                                       stage_run=StageRun,
                                                       scope = Scope,
                                                       skip_phase_groups=SkippedPhases,
                                                       project_config=ProjectConfig} = State) when is_list(PhaseItem) ->
    [EntName] = deliv_scopes:'#get'([ent_name], Scope),
    log(debug, State, "Creating phase group ~p", [PhaseItem]),
    RunningPhasesList = lists:foldl(
        fun(PhaseAtom, PhasesList) ->
            Phase = chef_utils:to_bin(PhaseAtom),
            lists:foldl(
                fun({SearchQuery, SearchDescription}, InnerPhasesList) ->
                    Skip = lists:member(Phase, SkippedPhases),
                    Timeout = deliv_proj_config:get_timeout_for_phase(ProjectConfig, Phase),
                    case deliv_phase:start_link(Change, Patchset, StageRun,
                                               self(), Phase, SearchQuery,
                                               SearchDescription, Skip, ?JOB_DISPATCH_V1,
                                               undefined, Timeout) of
                        {ok, Pid} -> [Pid | InnerPhasesList];
                        {error, normal} -> InnerPhasesList
                    end
                end,
                PhasesList,
                queries_for_phase(Phase, ProjectConfig, EntName)
            )
        end,
        [],
        PhaseItem
    ),
    State#deliv_stage{running_phases=RunningPhasesList};
create_phases_from_phase_group(?JOB_DISPATCH_V2, SinglePhase, State) when is_atom(SinglePhase) ->
    create_phases_from_phase_group(?JOB_DISPATCH_V2, [SinglePhase], State);
create_phases_from_phase_group(?JOB_DISPATCH_V2, PhaseGroup, State) ->
    deliv_stage_project_v2:create_phases_from_phase_group(PhaseGroup, State).

%% @doc Returns the search query to get the builders for given phase
-spec queries_for_phase(binary(), tuple(), binary()) -> [tuple()].
queries_for_phase(Phase, Config, EntName) ->
    case deliv_proj_config:get_searches_for_phase(Phase, Config) of
        [] ->
            EntId = get_ent_id(deliv_enterprise:fetch(EntName)),
            EntDefaultSearch = [Search || #deliv_enterprise_default_search{search = Search} <- deliv_enterprise_default_search:fetch(EntId)],
            [{default_search(EntDefaultSearch), undefined}];
        Searches ->
            handle_multiple_searches_for_phase(Searches)
    end.

get_ent_id({ok, Enterprise}) ->
    deliv_enterprise:getval(id, Enterprise);
get_ent_id({error, Why} = Params) ->
    chef_log:failed_call(?MODULE, get_ent_id, [Params], Why),
    erlang:exit(get_ent_id_failed).


%% @doc Returns the search query to get the builders for given phase
-spec handle_multiple_searches_for_phase([tuple()|binary()]) -> [tuple()].
handle_multiple_searches_for_phase(Searches) ->
  lists:foldl(fun(Search, Acc) when is_binary(Search) ->
                   lists:append(Acc, [{with_default_query(Search), undefined}]);
                 (Search, Acc) ->
                   Query = ej:get({<<"query">>}, Search, default_search()),
                   Desc  = ej:get({<<"description">>}, Search),
                   lists:append(Acc, [{with_default_query(Query), Desc}])
              end,
              [],
              Searches
  ).

%% @private
%% @doc Appends the default query to any user-defined query
%% (simply restrains to build nodes)
-spec with_default_query(binary()) -> binary().
with_default_query(Query) ->
    DefaultSearch = default_search(),
    <<DefaultSearch/binary, " AND (", Query/binary, ")">>.

%% @private
-spec default_search() -> binary().
default_search() ->
    chef_utils:to_bin(delivery_app:get_env(deliv_default_search)).

%% @private
-spec default_search([list()]) -> binary().
default_search([]) ->
    chef_utils:to_bin(delivery_app:get_env(deliv_default_search));
default_search(EntDefaultSearch) ->
    chef_utils:to_bin([delivery_app:get_env(deliv_default_search), " AND ", EntDefaultSearch]).

%% @private
%% @doc Runs the phases
-spec run_phases(#deliv_stage{}) -> #deliv_stage{}.
run_phases(#deliv_stage{stage_run=StageRun,
                        running_phases=RunningPhases} = State) ->
    lists:foreach(
        fun(PhasePid) -> deliv_phase:run(PhasePid) end,
        RunningPhases
    ),
    {ok, NewStageRun} = deliv_stage_run:start(StageRun),
    NewState = State#deliv_stage{stage_run=NewStageRun},
    publish_stage_running(NewState),
    NewState.

%% @private
%% @doc After a successful change, triggers the next one
%% if any has been set in the stages' data
-spec trigger_next_stage(deliv_stage_name(), d_change()) -> ok.
trigger_next_stage(CurrentStageName, Change) ->
    case next_stage(CurrentStageName) of
        none ->
            %% nothing to do
            ok;
        NextStage ->
            ok = trigger(NextStage, Change)
    end.

%% @private
-spec next_stage(deliv_stage_name()) -> deliv_stage_name() | none.
next_stage(CurrentStageName) ->
    {_StagePhases, NextStage} = stage_data_for(CurrentStageName),
    NextStage.

%% @private
%% @doc Queues the stage for ulterior processing
-spec queue_stage(#deliv_stage{}) -> ok.
queue_stage(State) ->
    deliverance_stage_queue:in(State).

%% @private
%% @doc Utility function to add some meta to log messages
-spec log(lager:log_level(), #deliv_stage{}, string()) -> _.
log(LogLevel, State, MsgFormat) ->
    log(LogLevel, State, MsgFormat, []).

%% @private
%% @doc Same as `log/3', with some format args
-spec log(lager:log_level(), #deliv_stage{}, string(), list()) -> _.
log(LogLevel, #deliv_stage{stage_run=StageRun,
                           scoping_names=[Ent, Org, Proj, Pipe]}, MsgFormat, FormatArgs) ->
    Format = "Stage ~s for change ~s on ~s/~s/~s/~s: " ++  MsgFormat,
    Args = [deliv_stage_run:getval(stage, StageRun),
            deliv_stage_run:getval(change_id, StageRun),
            Ent, Org, Proj, Pipe | FormatArgs],
    chef_log:log(LogLevel, Format, Args).

%% @private
%% @doc send an event to notify that a stage run failed.
-spec notify_failure(deliv_stage_name(), binary(), [binary()]) -> any().
notify_failure(StageName, ChangeId, [Ent, Org, Proj, _Pipe]) ->
    ChangeUrl = deliv_web_utils:make_web_url_for_change(Ent, Org, Proj, ChangeId),
    StageNameStr = atom_to_list(StageName),
    Msg = [StageNameStr,
           " stage unsuccessful for change: ",
           ChangeUrl,
           "/status/",
           StageNameStr],
    % {stage_run, failures} is currently the only key supported by deliv_hipchat_notify
    deliv_event:publish({stage_run, failures}, lists:flatten(io_lib:format("~s", [Msg]))).

%% @private
%% @doc Event for the streaming named change endpoint
-spec publish_build_event(#deliv_stage{}) -> true.
publish_build_event(#deliv_stage{change = Change}) ->
    ChangeId = deliv_change:getval(id, Change),
    %% TODO: We should look at enhancing the event modules so we don't have to
    %% publish two events here.
    deliv_event:publish({build_event_for_change, ChangeId}, Change),
    deliv_event:publish(build_event_for_change, Change).

publish_stage_started(Stage) ->
    publish_stage_event(Stage, <<"running">>, started).

publish_stage_finished(#deliv_stage{stage_name = union}=Stage, Status) ->
    publish_union_finished_event(Stage),
    publish_stage_event(Stage, Status, finished);
publish_stage_finished(Stage, Status) ->
    publish_stage_event(Stage, Status, finished).

publish_union_finished_event(#deliv_stage{consumer_change_ids=ConsumerChangeIds,
                                          failed_pipelines=FailedPipelineIds,
                                          passed_pipelines=PassedPipelineIds}) ->

    ConsumerPipelineList = lists:map(fun(ChangeId) ->
                    {ok, Change} = deliv_change:fetch_by_id(ChangeId),
                    PipelineId = deliv_change:getval(pipeline_id, Change),
                    {PipelineId, unknown}
              end,
              ConsumerChangeIds),
    AllMap = maps:from_list(ConsumerPipelineList),
    PassedMap = maps:from_list([{P, passed} || P <- PassedPipelineIds]),
    FailedMap = maps:from_list([{P, failed} || P <- FailedPipelineIds]),

    FinalMap = maps:merge(maps:merge(AllMap, PassedMap), FailedMap),

    deliv_event:publish(union_finished, FinalMap).

publish_stage_running(Stage) ->
    publish_stage_event(Stage, <<"running">>, running).

publish_stage_event(#deliv_stage{stage_name = StageName}=Stage, Status, Action) ->
    EventPayload = stage_to_stage_event_payload(Stage, Status, Action),
    deliv_event:publish({{stage,Action}, StageName}, EventPayload).

%% @doc Utility function to translate between internal FSM state record and public
%% stage-started event.
stage_to_stage_event_payload(#deliv_stage{stage_name=StageName,
                                            scope=Scope,
                                            stage_run=StageRun,
                                            change=Change}, Status, Action) ->
    #stage_event{action = Action,
                 create_time = calendar:now_to_universal_time(os:timestamp()),
                 stage_name=StageName,
                 status=Status,
                 scope=Scope,
                 stage_run=StageRun,
                 change=Change}.

subscribe_stage_events() ->
    SubscriptionKeys = lists:flatten([
                                [
                                 {{stage, started}, StageName},
                                 {{stage, finished}, StageName},
                                 {{stage, running}, StageName}
                                ]
              || {StageName, _} <- deliv_stage_data:stages()]),
    deliv_event:subscribe(SubscriptionKeys).

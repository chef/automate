%% Transforms phase_run_summary records into JSON.

-module(deliv_phase_run_summary).

-include("deliv_types.hrl").

-export([
         to_ejson/1,
         to_ejson/2,
         to_ejson/3
        ]).

%% Helper types for specs
-type grouped_phases() :: {binary(), binary(),
    [#phase_run_summary{} |grouped_phases()]}.
-type phase_run_url_fun() :: fun((integer()) -> binary()) | undefined.
-type phase_run_job_url_fun() :: fun((integer(), integer()) -> binary()) | undefined.

%% @doc Generates JSON structures for a list of phase_run_summary records.
%% Records are grouped by stage, then by phase. Returns a list of JSON:
%% [{
%%   stage: STAGE_NAME,
%%   status: STAGE_STATUS,
%%   phases: [
%%     {
%%       phase: PHASE_NAME,
%%       status: PHASE_STATUS,
%%       run_details: [
%%         {
%%           phase: PHASE_NAME,
%%           status: PHASE_RUN_STATUS
%%         }, ...
%%       ]
%%     }, ...
%%   ]
%% }, ...]
-spec to_ejson(Input :: {ok, [#phase_run_summary{}]} | {error, term()}) ->
    {ok, [ej:json_object()]} | {error, term()}.
to_ejson(Input) ->
    to_ejson(Input, undefined, undefined).

-spec to_ejson(Input :: {ok, [#phase_run_summary{}]} | {error, term()},
               phase_run_url_fun()) ->
    {ok, [ej:json_object()]} | {error, term()}.
to_ejson(Input, UrlFun) ->
    to_ejson(Input, UrlFun, undefined).

%% @doc Generates JSON structures for a list of phase_run_summary records.
%% Records are grouped by stage, then by phase. Uses the url generator function
%% to create hrefs for each record. Returns a list of JSON:
%% [{
%%   stage: STAGE_NAME,
%%   status: STAGE_STATUS,
%%   phases: [
%%     {
%%       phase: PHASE_NAME,
%%       status: PHASE_STATUS,
%%       run_details: [
%%         {
%%           status: PHASE_STATUS,
%%           description: DESCRIPTION,
%%           href: URL,
%%           job_href: URL,
%%           search_query: QUERY,
%%           search_description: QUERY_DESCRIPTION
%%         }, ...
%%       ]
%%     }, ...
%%   ]
%% }, ...]
-spec to_ejson(Input :: {ok, [#phase_run_summary{}]} | {error, term()},
               phase_run_url_fun(),
               phase_run_job_url_fun()) -> {ok, [ej:json_object()]} | {error, term()}.
to_ejson({ok, []}, _UrlFun, _JobUrlFun) ->
    {ok, []};
to_ejson({ok, ResultSet}, UrlFun, JobUrlFun) ->
    Grouped = group_by_stage(ResultSet, []),
    {ok, [stage_group_to_ejson(GP, UrlFun, JobUrlFun) || GP <- Grouped]};
to_ejson({error, Error}, _UrlFun, _JobUrlFun) ->
    {error, Error}.

%% @private
%% @doc Group `phase_run_summary' records by stage, then by phase. Preserves
%% ordering, as it is assumed that the records are already in the proper order
%% (sorted by the database).

%% The final structure has the form:
%%   [{StageName, StageStatus, [{PhaseName, PhaseStatus, [PhaseRunSummary, ...]},
%%                              {PhaseName, PhaseStatus, [PhaseRunSummary, ...]},
%%                              ...]},
%%    {StageName, StageStatus, [{PhaseName, PhaseStatus, [PhaseRunSummary, ...]},
%%                              {PhaseName, PhaseStatus, [PhaseRunSummary, ...]},
%%                              ...]}
%%   ]
%% where each PhaseRunSummary in {PhaseName, PhaseStatus, [PhaseRunSummary, ...]}
%% has phase=PhaseName, and PhaseStatus is:
%%   <<"failed">> if status=<<"failed">> for some PhaseRunSummary, OR
%%   <<"running">> if status=<<"running">> for some PhaseRunSummary, OR
%%   <<"passed">> if status=<<"passed">> for every PhaseRunSummary
-spec group_by_stage([#phase_run_summary{}], [grouped_phases()]) -> term().
group_by_stage([#phase_run_summary{stage=NewStage, stage_status=StageStatus}=Sum|T], []) ->
    %% Processing will start in this function head
    group_by_stage(T, [{NewStage, StageStatus, [create_new_phase_group(Sum)]}]);
group_by_stage([#phase_run_summary{stage=Stage, stage_status=StageStatus}=Sum|T],
        [{Stage, StageStatus, PhaseList} | Others]) ->
    %% Continue processing phases for the current stage.
    %% Add this phase run summary to the stage's grouped phases list.
    GroupedPhases = add_to_phase_group(Sum, PhaseList),
    group_by_stage(T, [{Stage, StageStatus, GroupedPhases} | Others]);
group_by_stage([#phase_run_summary{stage=NewStage, stage_status=NewStatus}=Sum|T],
        [{OldStage, OldStatus, PhaseList} | Others]) ->
    %% Begin grouping phases for a new stage, NewStage.
    %% Grouped phases are prepended to the grouped phases list as they are processed.
    %% Reverse the grouped phases list to preserve phase ordering.
    group_by_stage(T, [{NewStage, NewStatus, [create_new_phase_group(Sum)]},
        {OldStage, OldStatus, lists:reverse(PhaseList)} | Others]);
group_by_stage([], [{FinalStage, FinalStatus, PhaseList} | Rest]) ->
    %% End of the line; reverse the final stage's phases, then reverse
    %% all the stages themselves
    [{Phase, PhaseStatus, GroupedPhaseList} | Others] = PhaseList,
    %% Reverse the last phase group list
    PhaseList2 = [{Phase, PhaseStatus, lists:reverse(GroupedPhaseList)} | Others],
    lists:reverse([{FinalStage, FinalStatus, lists:reverse(PhaseList2)} | Rest ]).

%% @private
%% Creates a new phase group tuple for grouping phases.
create_new_phase_group(#phase_run_summary{phase=Phase, phase_status=Status} = Sum) ->
    {Phase, Status, [Sum]}.

%% @private
%% Helper function for group_by_stage. Given a phase_run_summary and a list of
%% grouped phases:
%%   if phase_run_summary phase matches the phase of the latest phase group:
%%     - adds the phase_run_summary to the phase group's grouped phases lists
%%     - updates the phase group's status
%%   otherwise,
%%     - adds a new phase group tuple to the grouped phases list.
%%
%% Examples:
%% add_to_phase_group(#phase_run_summary{phase=<<"unit">>, status=<<"running">>} = Sum,
%%                [{<<"unit">>, <<"passed">>, [...]}]) =>
%%   [{<<"unit">>, <<"running">>, [Sum, ...]}]
%%
%% add_to_phase_group(#phase_run_summary{phase=<<"lint">>, status=<<"passed">>} = Sum,
%%                [{<<"unit">>, <<"running">>, [Sum2, ...]}]) =>
%%   [{<<"lint">>, <<"passed">>, [Sum, ...]},
%%    {<<"unit">>, <<"running">>, [..., Sum2]}]
add_to_phase_group(#phase_run_summary{phase=Phase, phase_status=PhaseStatus} = Sum,
        [{Phase, GroupedPhaseStatus, PhaseList} | Others]) ->
    NewGroupedPhaseStatus = grouped_phase_status(PhaseStatus, GroupedPhaseStatus),
    [{Phase, NewGroupedPhaseStatus, [Sum | PhaseList]} | Others];
add_to_phase_group(Sum, [{OldPhase, Status, OldGroupedPhases} | Others]) ->
    ReversedGroupedPhases = lists:reverse(OldGroupedPhases),
    [create_new_phase_group(Sum) | [{OldPhase, Status, ReversedGroupedPhases} | Others ]].

%% @private
%% Helper function for add_to_phase_group. Given two phase statuses, returns the
%% phase status with the highest precedence.
%% Precedences from low to high:
%%   <<"skipped">> =:= <<"passed">>, <<"idle">>, <<"running">>, <<"failed">>
grouped_phase_status(PhaseStatus, OtherPhaseStatus) when
        PhaseStatus =:= <<"failed">> ; OtherPhaseStatus =:= <<"failed">> ->
    <<"failed">>;
grouped_phase_status(PhaseStatus, OtherPhaseStatus) when
        PhaseStatus =:= <<"running">> ; OtherPhaseStatus =:= <<"running">> ->
    <<"running">>;
grouped_phase_status(PhaseStatus, OtherPhaseStatus) when
        PhaseStatus =:= <<"idle">> ; OtherPhaseStatus =:= <<"idle">> ->
    <<"idle">>;
grouped_phase_status(<<"skipped">>, <<"skipped">>) ->
    <<"skipped">>;
grouped_phase_status(_, _) ->
    <<"passed">>.

%% @private Generates JSON for a stage
stage_group_to_ejson({Stage, StageStatus, GroupedPhases}, UrlFun, JobUrlFun) ->
    {[
        {<<"stage">>, Stage},
        {<<"status">>, StageStatus},
        {<<"phases">>,
            [phase_group_to_ejson(PhaseGroup, UrlFun, JobUrlFun) || PhaseGroup <- GroupedPhases]}
    ]}.

%% @private Generates JSON for phase_run_summary groups, by phase
phase_group_to_ejson({PhaseName, PhaseStatus, PhaseSummaryList}, UrlFun, JobUrlFun) ->
    {[
        {<<"name">>, PhaseName},
        {<<"status">>, PhaseStatus},
        {<<"run_details">>,
            [phase_run_summary_to_ejson(PhaseSummary, UrlFun, JobUrlFun) || PhaseSummary <- PhaseSummaryList]}
    ]}.

%% @private Generates JSON for a phase_run_summary
phase_run_summary_to_ejson(#phase_run_summary{phase = Phase,
                                              phase_status = Status}, undefined, undefined) ->
    %% TODO: We likely want to return the description here instead of phase.
    {[
      {<<"phase">>, Phase},
      {<<"status">>, Status}
     ]};
phase_run_summary_to_ejson(#phase_run_summary{
                              description = Description,
                              phase_id = PhaseId,
                              phase_status = Status,
                              search_description = SearchDescription,
                              search_query = SearchQuery},
                           UrlFun, undefined) ->
    {[
      {<<"status">>, Status},
      {<<"description">>, Description},
      {<<"href">>, UrlFun(PhaseId)},
      {<<"search_query">>, SearchQuery},
      {<<"search_description">>, SearchDescription}
     ]};
phase_run_summary_to_ejson(#phase_run_summary{
                              description = Description,
                              phase_id = PhaseId,
                              phase_status = Status,
                              stage_id = StageId,
                              search_description = SearchDescription,
                              search_query = SearchQuery},
                           UrlFun, JobUrlFun) ->
    {[
      {<<"status">>, Status},
      {<<"description">>, Description},
      {<<"href">>, UrlFun(PhaseId)},
      {<<"job_href">>, JobUrlFun(StageId, PhaseId)},
      {<<"search_query">>, SearchQuery},
      {<<"search_description">>, SearchDescription}
     ]}.

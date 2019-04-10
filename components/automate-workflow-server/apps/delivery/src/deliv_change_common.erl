%% @doc Common code needed for generating JSON output for both the
%% deliv_hand_change and deliv_hand_changes Cowboy handlers
-module(deliv_change_common).

-include("deliv_types.hrl").

-export([
         get_change_as_ejson/5,
         get_changes_as_ejson/5,
         promotion_status_ejson/1
        ]).

-ifdef(TEST).
-compile([export_all]).
-endif.

get_changes_as_ejson(EntName, OrgName, ProjName, UserName, SortAndFilterParameters) ->
    case deliv_project:changes(EntName, OrgName, ProjName, UserName, SortAndFilterParameters) of
        {ok, Changes} ->
            changes_to_ejson({EntName, OrgName, ProjName}, UserName, Changes);
        {error, _} = Error ->
            Error
    end.

-spec get_change_as_ejson(binary(), binary(), binary(), binary(), binary())
                         -> {ok, json()} | {error, not_found | _Other}.
get_change_as_ejson(EntName, OrgName, ProjName, UserName, ChangeId) ->
    case deliv_change:fetch_by_id(ChangeId) of
        {ok, Change} ->
            change_to_ejson({EntName, OrgName, ProjName}, UserName, Change);
        {error, _} = Error ->
            Error
    end.

%% Ejson Processing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% There's a good bit of processing here! The main idea is to either
%% return {ok, ChangeEjson} or the first error that we encounter in
%% the course of building up that Ejson.
%%
%% For each change, we'll need to retrieve all patchsets for the
%% change. For each patchset, we'll need to retrieve all commit
%% messages from the commits on that patchset's branch, as well as the
%% cumulative diffstats for that patchset. Only if all of that works
%% do we want to return the Ejson. Furthermore, we don't want to
%% continue processing information once we've received an error.
%%
%% Function Patterns:
%%
%% handle_FOOs: handle results of retrieving data from another system (database or Git)
%% FOOs_to_ejson: convert valid retrieved FOOs into Ejson
%% FOOs_to_ejson_acc: accumulate valid Ejson conversions; short-circuit at first error
%% FOO_to_ejson: convert a single FOO to ejson
%% handle_FOO: called from FOO_to_ejson: determine ok / error status based on inputs

changes_to_ejson(RepoCoordinates, UserName, Results) ->
    %% Results is db resultset
    changes_to_ejson_acc(RepoCoordinates, UserName, Results, []).

changes_to_ejson_acc(_RepoCoordinates, _UserName, [], EjsonAcc) ->
    {ok, lists:reverse(EjsonAcc)};
changes_to_ejson_acc(RepoCoordinates, UserName, [Result|Results], EjsonAcc) ->
    case change_to_ejson(RepoCoordinates, UserName, Result) of
        {ok, Ejson} ->
            changes_to_ejson_acc(RepoCoordinates, UserName, Results, [Ejson|EjsonAcc]);
        {error, Error} ->
            {error, Error}
    end.

change_to_ejson({EntName, OrgName, ProjName} = RepoCoordinates, UserName, Result) ->
    ChangeId      = deliv_change:getval(id, Result),
    Pipeline      = deliv_change:getval(pipeline_name_at_creation, Result),
    FeatureBranch = deliv_change:getval(feature_branch, Result),

    %% This seems like the least-horrible way to do this
    PhaseRunUrlCreator = fun(PhaseId) ->
                                 deliv_web_utils:relative_href_for(phase_run, [EntName, OrgName, ProjName, Pipeline, PhaseId])
                         end,
    PhaseRunJobUrlCreator = fun(StageId, PhaseId) ->
                                    JobId = deliv_job_command:generate_id(StageId, PhaseId, ChangeId),
                                    deliv_web_utils:relative_href_for(phase_run_job, [EntName, JobId])
                            end,

    PhaseRunSummary      = deliv_change:get_phase_run_summary(ChangeId),
    PhaseRunSummaryEjson = deliv_phase_run_summary:to_ejson(PhaseRunSummary,
                                                            PhaseRunUrlCreator,
                                                            PhaseRunJobUrlCreator),

    Patchsets     = deliv_patchset:patchsets_for_change(ChangeId),
    PatchsetEjson = handle_patchsets(Patchsets, RepoCoordinates, {ChangeId, Pipeline, FeatureBranch}),

    SupersedingChange = deliv_change:get_superseding_change(Result),
    SupersedingChangeEjson = handle_superseding_change(SupersedingChange),

    HAL           = generate_change_hal(RepoCoordinates, Pipeline, UserName, Result),

    case handle_change(Result, PhaseRunSummaryEjson, PatchsetEjson, SupersedingChangeEjson) of
        {ok, Result1} ->
            {ok, {Result1 ++ HAL}};
        {error, _} = Error ->
            Error
    end.

generate_change_hal(RepoCoordinates, Pipeline, UserName, Result) ->
    ChangeId      = deliv_change:getval(id, Result),
    ChangeStatus  = deliv_change:getval(latest_patchset_status, Result),

    Hal = case deliv_change:get_stage_run_summary(ChangeId) of
        {ok, StageRunSummaries} ->
            %% All other cases process StageRunSummary to produce HAL.
            deliv_stage_run_summary:to_hal(StageRunSummaries,
                ChangeStatus, RepoCoordinates, Pipeline, UserName, ChangeId);
        {error, _} ->
            {[]}
    end,

    {EntName, OrgName, ProjName} = RepoCoordinates,
    {ok, Project} = deliv_project:fetch(EntName, OrgName, ProjName),
    ScmModule = chef_utils:to_atom(deliv_project:getval(scm_module, Project)),
    Hal2 = case ScmModule of
        bitbucket_scm ->
            HalMetaData = scm_change:get_hal(ChangeId, ScmModule),
            ej:set_p([<<"external_pr">>], Hal, HalMetaData);
        github_scm ->
            HalMetaData = scm_change:get_hal(ChangeId, ScmModule),
            ej:set_p([<<"external_pr">>], Hal, HalMetaData);
        _ -> Hal
    end,

    [{<<"_links">>, Hal2}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Patchsets
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_patchsets(Patchsets, RepoCoordinates, ChangeCoordinates) when erlang:is_list(Patchsets)->
    patchsets_to_ejson(RepoCoordinates, ChangeCoordinates, Patchsets);
handle_patchsets({error, PatchsetError}, _RepoCoordinates, _ChangeCoordinates) ->
    {error, PatchsetError}.

%% Patchsets = list of records

%% @doc Returns a list of patchset Ejson data (in an 'ok' tuple) or
%% {error, Reason} if something bad happened trying to assemble said
%% data.
patchsets_to_ejson(_, {ChangeId, _, _}, Patchsets) ->
    patchsets_to_ejson_acc(deliv_scopes:from_change_id(ChangeId), Patchsets, []).

patchsets_to_ejson_acc(_Scope, [], EjsonAcc) ->
    {ok, lists:reverse(EjsonAcc)};
patchsets_to_ejson_acc(Scope, [Patchset|Patchsets], EjsonAcc) ->
    case deliv_patchset:to_ejson(Scope, Patchset) of
        {ok, Ejson} ->
            patchsets_to_ejson_acc(Scope, Patchsets, [Ejson|EjsonAcc]);
        {error, _} = Error ->
            Error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Changes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_change(Result, {ok, PhaseRunSummaryEjson}, {ok, PatchsetEjson}, SupersedingChangeEjson) ->
    ChangeEjsonValues = ejson_values_from_change(Result),
    OtherEjsonValues = [
                        {<<"promotion">>,    handle_promotion_status(Result)},
                        {<<"stages">>,       PhaseRunSummaryEjson},
                        {<<"patchsets">>,    PatchsetEjson},
                        {<<"superseding_change">>, SupersedingChangeEjson}
                       ],
    {ok, ChangeEjsonValues ++ OtherEjsonValues };
handle_change(_Result, {error, PhaseRunSummaryError}, _PatchsetEjson, _SupersedingChangeEjson) ->
    {error, PhaseRunSummaryError};
handle_change(_Result, _PhaseRunSummaryEjson, {error, PatchsetEjsonError}, _SupersedingChangeEjson) ->
    {error, PatchsetEjsonError}.

ejson_values_from_change(Change) ->
    [
     {<<"id">>,           deliv_change:getval(id, Change)},
     {<<"topic">>,        deliv_change:getval(feature_branch, Change)},
     {<<"target">>,       deliv_change:getval(pipeline_name_at_creation, Change)},
     {<<"state">>,        deliv_change:getval(latest_patchset_status, Change)},
     {<<"submit_at">>,    chef_utils:format_timestamp(deliv_change:getval(submitted_at, Change))},
     {<<"submit_by">>,    deliv_change:getval(submitted_by, Change)},
     {<<"merge_sha">>,    deliv_change:getval(merge_sha, Change)},
     {<<"approved_by">>,  deliv_change:getval(approved_by, Change)},
     {<<"title">>,        deliv_change:getval(title, Change)},
     {<<"description">>,  deliv_change:getval(description, Change)},
     {<<"delivered_by">>, deliv_change:getval(delivered_by, Change)},
     {<<"delivered_at">>, case deliv_change:getval(delivered_at, Change) of
                              undefined -> <<"">>;
                              DeliveredAt ->
                                  chef_utils:format_timestamp(DeliveredAt)
                          end}
    ].

handle_superseding_change({ok, SupersedingChange}) ->
    {ejson_values_from_change(SupersedingChange)};
handle_superseding_change(undefined) ->
    undefined.

%% @doc Return the ejson that indicates whether or not it is safe to promote
%% this change.
handle_promotion_status(Change) ->
    promotion_status_ejson(deliv_change:status(Change)).

-spec promotion_status_ejson(change_status()) -> json().
promotion_status_ejson(blocked) ->
    format_promotion_status(<<"caution">>, <<"pipeline_union_failure">>);
promotion_status_ejson(delivered) ->
    format_promotion_status(<<"disabled">>, <<"change_delivered">>);
promotion_status_ejson(superseded) ->
    format_promotion_status(<<"disabled">>, <<"change_superseded">>);
promotion_status_ejson(Status) when Status =:= open; Status =:= approved->
    format_promotion_status(<<"proceed">>, undefined).

%% @private
format_promotion_status(Status, undefined) ->
    {[
        {<<"status">>, Status}
    ]};
format_promotion_status(Status, Reason) ->
    {[
        {<<"status">>, Status},
        {<<"reason">>, Reason}
    ]}.

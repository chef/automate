-module(deliv_change).
-behaviour(deliv_sqerl_rec).

-include("deliv_types.hrl").

-compile({parse_transform, sqerl_gobot}).

%% DB operations
%% no insert, changes are created only when creating patchsets!
-export([
         delete/1,
         fetch_by_id/1,
         update/1,
         subscribe_change_events/1,
         subscribe_change_events/0
        ]).

%% sqerl callbacks
-export([
        '#insert_fields'/0,
        '#update_fields'/0,
        '#statements'/0,
        '#table_name'/0
       ]).

%% deliv_sqerl_rec callbacks
-export([
        scoping_parent/0,
        scoping_column_name/0,
        scoping_index_constraint/0
       ]).

%% others
-export([
         scoping_names/1,
         merge/2,
         accept/2,
         verify_patchset/1,
         trigger_stage/2,
         changes/4,
         get_changeset_status/1,
         get_phase_run_summary/1,
         get_stage_run_summary/1,
         is_merged/1,
         get_superseding_change/1,
         delete_change_and_branch/2,
         changelog_for_changeset/1,
         changelog_for_open_changeset/1,
         validate_merge/1,
         validate_accept/1,
         set_superseded_changes/1,
         status/1
        ]).

-ifdef(TEST).
-compile(export_all).
-endif.

-record(deliv_change, {
          id                        :: db_guid(),
          pipeline_id               :: db_id(),
          feature_branch            :: binary(),
          merge_sha                 :: binary(),
          title                     :: binary(),
          description               :: binary(),
          approved_by               :: binary(),
          approved_at               :: calendar:datetime(),
          changeset_id              :: db_guid(),
          latest_patchset_status    :: binary(),
          latest_patchset           :: non_neg_integer(),
          submitted_at              :: calendar:datetime(),
          submitted_by              :: binary(),
          delivered_at              :: calendar:datetime(),
          delivered_by              :: binary(),
          pipeline_name_at_creation :: binary(),
          superseding_change_id     :: db_guid()
         }).

%% Those are not actually used since we do
%% all the insert/update operations through custom DB funs
'#insert_fields'() -> [pipeline_id].
'#update_fields'() -> [merge_sha, title, description].

'#statements'() ->
    %% we use at least `deliv_change_fetch_by_id' from the default queries
    [default,
     {get_phase_run_summary,
      <<"SELECT * FROM phase_run_summary($1)">>},
     {get_stage_run_summary,
      <<"SELECT * FROM stage_run_summary($1)">>},
     {update_change_for_merge,
      <<"SELECT * FROM merge_change($1, $2, $3)">>},
     {changeset_status_for_changeid,
      <<"SELECT cs.status "
          "FROM changesets AS cs "
          "JOIN changes AS c "
            "ON cs.id = c.changeset_id "
         "WHERE c.id = $1">>},
     {changelog_for_changeset,
      <<"SELECT * "
          "FROM changes "
         "WHERE changeset_id = $1">>},
     {changelog_for_open_changeset,
      <<"SELECT c.* "
          "FROM changes AS c "
          "JOIN changesets AS cs "
            "ON cs.id = c.changeset_id "
         "WHERE cs.pipeline_id = $1 "
        "AND cs.status = 'open'">>},
     {get_changes,
      %% $1 = enterprise name (NOT NULL)
      %% $2 = organization name (NOT NULL)
      %% $3 = project name (NOT NULL)

      %% The remaining arguments are for filtering / sorting, and can all be NULL
      %% $4 = pipeline name (NULL-able)
      %% $5 = state (NULL-able)
      %% $6 = last change ID (NULL-able)
      %% $7 = reverse chronological sort (boolean, NULL-able)
      %% NOTE: While below is nullable, the stored procedure defaults to
      %% returning 10 when no limit is specified.
      %% $8 = limit (integer, NULL-able)
      %%
      %% TODO: will probably need to add user_name as an argument later
        <<"SELECT *
           FROM get_changes($1, $2, $3, $4, $5, $6, $7, $8)">> },
     {set_superseded_changes,
        <<"SELECT * FROM set_superseded_changes($1)">>},
     deliv_sqerl_rec:gen_fetch_scoping_params_from_id(?MODULE)].

'#table_name'() ->
    "changes".

-spec scoping_parent() -> {atom(), atom()}.
scoping_parent() ->
    {deliv_pipeline, pipeline_id}.

-spec scoping_column_name() -> feature_branch.
scoping_column_name() -> feature_branch.

-spec scoping_index_constraint() -> string().
scoping_index_constraint() -> "changes.merge_sha IS NULL".

-spec delete(binary() | d_change()) -> ok | {error, _}.
delete(Change) ->
    deliv_db:delete(Change).

-spec fetch_by_id(binary()) -> {ok, d_change()} |
                               {error, not_found | term()}.
fetch_by_id(ChangeId) ->
    deliv_db:fetch_by_id(?MODULE, ChangeId).

%% Subscribe for specific change events
-spec subscribe_change_events(d_change()) -> true.
subscribe_change_events(Change) ->
  deliv_event:subscribe({change_updated, getval(id, Change)}).

%% Subscribe for any change events
%% TODO: Right now, change_delivered is being published by deliv_changeset when
%% it closes the changeset. In the future we should find a way for deliv_change
%% to own that process.
-spec subscribe_change_events() -> true.
subscribe_change_events() ->
  deliv_event:subscribe([change_updated, change_deleted, change_created,
                         change_approved, change_superseded, change_delivered]).

%% @doc Returns a proplist of proplists (i.e., a sqerl result set)
%% with phase run summary information for the change
-spec get_phase_run_summary(binary()) -> {ok, [#phase_run_summary{}]} |
                                         {error, term()}.
get_phase_run_summary(ChangeId) ->
    deliv_db:select(?MODULE, get_phase_run_summary,
                    [ChangeId],
                    rows_as_records,
                    [phase_run_summary,
                     record_info(fields, phase_run_summary)]).

%% @doc Returns a list of stage_run_summary records. These are created through
%% sqerl by calling a stored proc in the db.
-spec get_stage_run_summary(binary()) -> {ok, [#stage_run_summary{}]} |
                                         {error, term()}.
get_stage_run_summary(ChangeId) ->
    deliv_db:select(?MODULE, get_stage_run_summary,
                    [ChangeId],
                    rows_as_records,
                    [stage_run_summary,
                     record_info(fields, stage_run_summary)]).

%% @doc Returns a proplist of proplists (i.e., a sqerl result set)
%% with phase run summary information for the change
-spec get_changeset_status(binary()) -> {ok, binary()} |
                                         {error, term()}.
get_changeset_status(ChangeId) ->
    case deliv_db:select(?MODULE, changeset_status_for_changeid, [ChangeId]) of
        {ok, []} ->
            {error, not_found};
        {ok, [[{<<"status">>, Result}]]} ->
            {ok, Result};
        {error, _} = Error ->
            Error
    end.

%% Update the database with the details for the given change.
-spec update(d_change()) -> {error, not_found} |
                            {ok, d_change()}.
update(Change) ->
    case deliv_db:update(Change) of
        {ok, UpdatedChange} = Result ->
            publish_event(change_updated, UpdatedChange),
            Result;
        Error -> Error
    end.

%% @doc Returns a list `[EntName, OrgName, ProjName, PipeName]'
-spec scoping_names(binary()) -> [binary()] | {error, _Why}.
scoping_names(ChangeId) ->
    deliv_sqerl_rec:fetch_scoping_params_from_id(?MODULE, ChangeId).

%% @doc After a change has been accepted, close the changeset and move to union.
-spec accept(ChangeId, User) -> ok | {error, Why} when
      ChangeId :: binary(),
      User     :: d_user(),
      Why      :: change_not_found | invalid_config | invalid_state | changeset_already_accepted | term().
accept(ChangeId, User) ->
    %% Validating status here because close_changeset is a direct db call that
    %% doesn't give all the error states we need. And we don't want to
    %% accidentally update the db.
    maybe_trigger_union(validate_accept(ChangeId),
                        deliv_changeset:close_changeset(ChangeId, User),
                        ChangeId).

%% @private
maybe_trigger_union(ok, ok, ChangeId) ->
    trigger_stage(union, ChangeId);
maybe_trigger_union(ok, CloseError, _) ->
    CloseError;
maybe_trigger_union(ValidationError, _, _) ->
    ValidationError.

%% @doc Merge the specified change.
-spec merge(binary() | d_change(), d_user()) -> ok | {error, _Why}.
merge(ChangeId, Approver) when is_binary(ChangeId) ->
    case fetch_by_id(ChangeId) of
        {ok, Change} ->
            merge(Change, Approver);
        {error, not_found} ->
            {error, change_not_found}
    end;
merge(#deliv_change{latest_patchset_status = <<"open">>, id = ChangeId} = Change, Approver) ->
    case validate_merge(ChangeId) of
        ok ->
            {ok, Patchset} = deliv_patchset:latest_patchset_for_change(ChangeId),
            Scope = deliv_scopes:from_change(Change),
            [ScmMod] = deliv_scopes:'#get'([scm_module], Scope),
            MergeResult = ScmMod:merge_feature_branch(Scope, Patchset, Approver),
            handle_scm_merge(MergeResult, Scope, Change, Patchset, Approver);
        {error, Why} = Error ->
            chef_log:failed_call(?MODULE, validate_merge, [ChangeId], Why),
            Error
    end;
merge(_Change, _Approver) ->
    {error, patchset_already_merged}.

%% @private Handle the result of the git merge (merge sha or failure)
handle_scm_merge({ok, MergeSha}, Scope, #deliv_change{id = ChangeId}, Patchset, Approver) ->
    [ScmMod] = deliv_scopes:'#get'([scm_module], Scope),
    Params = [ChangeId, MergeSha, deliv_user:getval(name, Approver)],
    case deliv_db:qfetch(?MODULE, update_change_for_merge, Params) of
        [UpdatedChange] ->
            case ScmMod:delete_feature_branch(Scope, Patchset) of
                {error, Why} ->
                    chef_log:info("Failed to delete feature branch for change(~s) - ~p", [ChangeId, Why]);
                _ -> ok
            end,
            deliv_patchset_changed_file:refresh_cache(Scope, UpdatedChange, Patchset),
            publish_event(change_approved, UpdatedChange),
            handle_trigger_stage(build, UpdatedChange);
        {error, UpdateReason} = UpdateError ->
            chef_log:failed_call(?MODULE, update_change_for_merge, Params, UpdateReason),
            UpdateError
    end;
handle_scm_merge({error, Why} = Error, Scope, _Change, Patchset, Approver) ->
    [ScmMod] = deliv_scopes:'#get'([scm_module], Scope),
    chef_log:failed_call(ScmMod, merge_feature_branch, [Scope, Patchset, Approver], Why),
    Error.

%% @doc Delete a change and its branch, but only on the condition
%% that is has not been merged; i.e. has no merge sha.
%% If anything goes wrong with the branch delete, we do not delete the change,
%% we log an error and abort.
-spec delete_change_and_branch(binary() | d_change(), d_user()) -> ok | {error, term()}.
delete_change_and_branch(ChangeId, Deleter) when is_binary(ChangeId) ->
    case fetch_by_id(ChangeId) of
        {ok, Change} ->
            delete_change_and_branch(Change, Deleter);
        {error, Why} = Error ->
            chef_log:failed_call(?MODULE, fetch_by_id, [ChangeId], Why),
            Error
    end;
delete_change_and_branch(#deliv_change{merge_sha = undefined, id = ChangeId} = Change, Deleter) ->
    {ok, LatestPatchset} = deliv_patchset:latest_patchset_for_change(ChangeId),
    Scope = deliv_scopes:from_change(Change),
    ScmMod = deliv_scopes:'#get'(scm_module, Scope),
    DeleteResult = ScmMod:delete_feature_branch(Scope, LatestPatchset),
    handle_delete_change_and_branch(DeleteResult, Change, Deleter);
delete_change_and_branch(#deliv_change{merge_sha = MergeSha, id = ChangeId}, _Deleter) ->
    chef_log:info("Can not delete merged change(~s); merge_sha:~s", [ChangeId, MergeSha]),
    {error, merged_change}.

%% @private Handle the result of the attempt to delete the git branch
handle_delete_change_and_branch({ok, _}, #deliv_change{id = ChangeId} = Change, Deleter) ->
    case delete(Change) of
        ok ->
            deliv_event:publish({change_deleted, ChangeId}, {deleted_by, Deleter}),
            deliv_event:publish(change_deleted, {{change_id, ChangeId}, {deleted_by, Deleter}}),
            ok;
        {error, Reason} = Error ->
            chef_log:failed_call(?MODULE, delete, [Change], Reason),
            Error
    end;
handle_delete_change_and_branch({error, Why}, #deliv_change{id = ChangeId}, _Deleter) ->
    chef_log:info("Failed to delete feature branch for change(~s) - ~p", [ChangeId, Why]),
    {error, feature_branch_delete_failed}.

%% @doc This function is provided specifically for use by
%% `deliv_hand_post_receive_hook' for the triggering of verify stage
%% runs in response to the creation of a new patchset. It is identical
%% to `trigger_stage/2', except that it unconditionally triggers a
%% Verify stage (as you would want; every new patchset should be
%% tested, regardless of what previous patchsets did).
verify_patchset(ChangeId) ->
    case fetch_by_id(ChangeId) of
        {ok, Change} ->
            deliv_stage:trigger(verify, Change);
        {error, not_found} ->
            {error, change_not_found}
    end.

%% @doc Conditionally triggers a stage run for a given change, based
%% on overall state of the change and pipeline. For instance, if a
%% Verify stage run fails, we would like to allow users to trigger a
%% re-run without having to submit a new patchset, but don't want to
%% allow re-triggers if the Verify stage's last run was
%% successful. Similar (but different!) logic applies to re-running
%% other stage runs.
%%
%% Note that Verify stage runs initiated by new patchset submission
%% bypasses this logic, instead  going through `verify_patchset/1'
-spec trigger_stage(deliv_stage_name(), binary()) -> ok | {error,
                                                           change_not_found |
                                                           invalid_config |
                                                           invalid_state |
                                                           term()}.
trigger_stage(StageName, ChangeId) when is_binary(ChangeId) ->
    case fetch_by_id(ChangeId) of
        {ok, Change} ->
            handle_trigger_stage(StageName, Change);
        {error, not_found} ->
            {error, change_not_found}
    end.

handle_trigger_stage(StageName, #deliv_change{id = ChangeId} = Change) ->
    case validate_trigger(StageName, Change) of
      ok ->
          deliv_stage:trigger(StageName, Change);
      Error ->
          chef_log:error("Stage '~s' not triggered for change '~s'; change is in an invalid state.",
                          [StageName, ChangeId]),
          Error
    end.

%% @doc Validates that a change is ina state to be merged.
-spec validate_merge(d_change() | binary()) -> ok | {error, invalid_state}.
validate_merge(#deliv_change{id = ChangeId}) ->
    validate_merge(ChangeId);
validate_merge(ChangeId) when is_binary(ChangeId) ->
    validate_trigger(build, ChangeId).

%% @doc Validates that a change is in a state to be accepted.
-spec validate_accept(d_change() | binary()) -> ok | {error, invalid_state | changeset_already_accepted}.
validate_accept(#deliv_change{id = ChangeId}) ->
    validate_accept(ChangeId);
validate_accept(ChangeId) when is_binary(ChangeId) ->
    case get_changeset_status(ChangeId) of
        {ok, <<"open">>} ->
           validate_trigger(union, ChangeId);
        {ok, <<"closed">>} ->
           {error, changeset_already_accepted};
        Error ->
            Error
    end.

-spec validate_trigger(deliv_stage_name(), d_change() | binary()) -> ok | {error, invalid_state}.
validate_trigger(StageName, #deliv_change{id = ChangeId}) ->
    validate_trigger(StageName, ChangeId);
validate_trigger(StageName, ChangeId) when is_binary(ChangeId) ->
    %% get_stage_run_summary returns a list of stage_run_summary records
    %% ordered with the latest one at the head of the lists. We only care
    %% about the latest.
    case {StageName, get_stage_run_summary(ChangeId)} of
        {verify, {ok, []}} -> ok;
        {verify, {ok, [#stage_run_summary{stage = <<"verify">>}]}} -> ok;
        {build, {ok, [#stage_run_summary{stage = <<"verify">>,
                                         status = <<"passed">>,
                                         finished = true} | _]}} -> ok;
        {build, {ok, [#stage_run_summary{stage = <<"build">>,
                                         status = <<"idle">>,
                                         finished = false,
                                         pipeline_latest = true}, _]}} -> ok;
        {build, {ok, [#stage_run_summary{stage = <<"build">>,
                                         status = <<"failed">>,
                                         pipeline_latest = true} | _]}} -> ok;
        {build, {ok, [#stage_run_summary{stage = <<"build">>,
                                         status = <<"running">>,
                                         finished = false,
                                         pipeline_latest = true} | _]}} -> ok;
        {acceptance, {ok, [#stage_run_summary{stage = <<"build">>,
                                              status = <<"passed">>,
                                              finished = true,
                                              pipeline_latest = true} | _]}} -> ok;
        {acceptance, {ok, [#stage_run_summary{stage = <<"acceptance">>,
                                              pipeline_latest = true} | _]}} -> ok;
        {union, {ok, [#stage_run_summary{stage = <<"acceptance">>,
                                         status = <<"passed">>,
                                         finished = true,
                                         pipeline_latest = true} | _]}} -> ok;
        {union, {ok, [#stage_run_summary{stage = <<"union">>,
                                         system_latest = true} | _]}} -> ok;
        {rehearsal, {ok, [#stage_run_summary{stage = <<"union">>,
                                             status = <<"passed">>,
                                             finished = true,
                                             system_latest = true} | _]}} -> ok;
        {rehearsal, {ok, [#stage_run_summary{stage = <<"rehearsal">>,
                                             system_latest = true} | _]}} -> ok;
        {delivered, {ok, [#stage_run_summary{stage = <<"rehearsal">>,
                                             status = <<"passed">>,
                                             finished = true,
                                             system_latest = true} | _]}} -> ok;
        {delivered, {ok, [#stage_run_summary{stage = <<"delivered">>,
                                             system_latest = true} | _]}} -> ok;
        {_, InvalidSummaryResult} ->
            chef_log:debug("Phase run summary for change ~s does not meet prerequisites to trigger ~s: ~p",
                        [ChangeId, StageName, InvalidSummaryResult]),
            {error, invalid_state}
    end.

%% @doc Returns true if the change has been merged
-spec is_merged(d_change()) -> boolean().
is_merged(#deliv_change{merge_sha=undefined}) -> false;
is_merged(#deliv_change{merge_sha=MergeSha})
              when erlang:is_binary(MergeSha) -> true.

%% @doc Returns the superseding change of a change or undefined
-spec get_superseding_change(d_change()) -> d_change() | undefined.
get_superseding_change(#deliv_change{superseding_change_id=SupersedingChangeId})
                       when erlang:is_binary(SupersedingChangeId) ->
    fetch_by_id(SupersedingChangeId);
get_superseding_change(_) ->
    undefined.

%% @doc Fetches all the changes for the given `ChangesetId'
-spec changelog_for_changeset(binary()) -> [d_change()] | {error, _Why}.
changelog_for_changeset(ChangesetId) ->
    deliv_db:qfetch(?MODULE, changelog_for_changeset, [ChangesetId]).

%% @doc Fetches all the changes for the currently open changeset
%% for the given `PipelineId'
-spec changelog_for_open_changeset(non_neg_integer())
        -> [d_change()] | {error, _Why}.
changelog_for_open_changeset(PipelineId) ->
    deliv_db:qfetch(?MODULE, changelog_for_open_changeset, [PipelineId]).

%% @doc Return custom change objects as dictated by the get_changes
%% stored procedure.
%% NOTE: Unless limit is specified otherwise, it will default to 10.
changes(EntName, OrgName, ProjectName, Params) ->

    %% The handler will pass a binary or undefined
    PipelineFilter = null_get(pipeline, Params),

    %% If we get "all", then that should be passed to the database function as NULL
    StatusFilter0 = null_get(state, Params),
    StatusFilter = case StatusFilter0 of
                       all -> null;
                       _ -> StatusFilter0
                   end,

    %% Handler will pass a binary or undefined
    LastChange = null_get(id, Params),

    %% API uses 'new_to_old' / 'old_to_new'; database function is in boolean terms
    %% The handler will always pass a value.
    SortOrder0 = null_get(sort, Params),
    SortOrder = case SortOrder0 of
                    new_to_old -> true;
                    old_to_new -> false;
                    _ -> SortOrder0
                end,

    %% The handler will always pass an integer here
    Limit = null_get(limit, Params),

    case deliv_db:qfetch(?MODULE, get_changes,
                          [EntName, OrgName, ProjectName,
                           PipelineFilter, StatusFilter, LastChange, SortOrder, Limit]) of
        Result when is_list(Result) ->
            {ok, Result};
        {error, Error} ->
            %% TODO translate possible error cases
            {error, Error}
    end.

%% @doc Return value if key-value pair exists, otherwise return `null`
null_get(Key, Params) ->
    proplists:get_value(Key, Params, null).

%% @doc Collects changes superseded by SupersedingChange and marks them
%% as superseded
-spec set_superseded_changes(d_change()) -> [d_change()].
set_superseded_changes(#deliv_change{id = ChangeId}) ->
    case deliv_db:qfetch(?MODULE, set_superseded_changes, [ChangeId]) of
        {error, Why} ->
            chef_log:failed_call(?MODULE, set_superseded_changes, [ChangeId], Why),
            [];
        Changes ->
            lists:foreach(fun(Change) ->
                publish_event(change_superseded, Change)
            end, Changes),
            Changes
    end.

%% @doc Return the current status of the change.
-spec status(d_change()) -> change_status().
status(#deliv_change{delivered_at = DeliveredAt})
            when erlang:is_tuple(DeliveredAt) -> delivered;
status(#deliv_change{superseding_change_id = SupersedingChangeId})
            when erlang:is_binary(SupersedingChangeId) -> superseded;
status(#deliv_change{pipeline_id = PipelineId} = Change) ->
    case deliv_dependency_failures:is_blocked(PipelineId) of
        true -> blocked;
        false -> change_status(Change)
    end.

change_status(#deliv_change{approved_by = ApprovedBy})
            when erlang:is_binary(ApprovedBy) -> approved;
change_status(#deliv_change{merge_sha = undefined}) -> open.

%% @private
publish_event(Event, #deliv_change{id = ChangeId} = Change) ->
    deliv_event:publish({Event, ChangeId}, Change),
    deliv_event:publish(Event, Change).

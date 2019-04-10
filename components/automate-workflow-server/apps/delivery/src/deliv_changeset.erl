%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et

-module(deliv_changeset).

-include("deliv_types.hrl").

-compile({parse_transform, sqerl_gobot}).

%% API Functions
-export([
         add_to_changeset/2,
         close_changeset/2,
         fetch_by_id/1,
         dependency_ejson_for_project/3,
         consumer_change_ids_for_pipeline/1,
         latest_closed_for_pipeline/1
        ]).

%% sqerl callbacks
-export([
        '#insert_fields'/0,
        '#update_fields'/0,
        '#statements'/0,
        '#table_name'/0
       ]).

-record(deliv_changeset, {
          id                :: db_id(),
          pipeline_id       :: db_id(),
          status            :: binary(),
          delivered_at      :: calendar:datetime(),
          delivered_by      :: binary(),
          dependencies      :: [db_id()],
          latest_change_id  :: db_guid()
         }).

'#insert_fields'() -> [id, pipeline_id, status, delivered_at, delivered_by, dependencies].
'#update_fields'() -> [status, delivered_at, delivered_by, dependencies].

'#statements'() ->
    [
     default,
     {add_to_changeset,
      <<"SELECT * FROM add_to_changeset($1, $2)">>},
     {close_changeset,
      %% $1 = change ID whose delivery closes the changeset
      %% $2 = name of the user that pushed the deliver button
      <<"WITH up AS (
      UPDATE changesets
                   SET status = 'closed',
                        delivered_by = $2,
                        delivered_at = NOW()
                  FROM changes
                 WHERE changes.id = $1
                   AND changesets.id = changes.changeset_id
                   AND changesets.status = 'open'
                   RETURNING changesets.*)
      UPDATE changes
      SET delivered_by = up.delivered_by,
          delivered_at = up.delivered_at
      FROM up
      WHERE changes.changeset_id = up.id">>},
     {dependencies_by_project,
      <<"SELECT * FROM dependencies_by_project($1, $2, $3)">>},
     {consumer_change_ids_by_pipeline,
      <<"SELECT * FROM consumer_change_ids_by_pipeline($1)">>},
     {latest_closed_for_pipeline,
      <<"SELECT * FROM changesets
         WHERE pipeline_id = $1
           AND status = 'closed'
         ORDER BY delivered_at DESC
         LIMIT 1">>}
    ].

'#table_name'() -> "changesets".

-spec fetch_by_id(binary()) -> {ok, d_change()} |
                               {error, not_found | term()}.
fetch_by_id(ChangeId) ->
    deliv_db:fetch_by_id(?MODULE, ChangeId).

-spec add_to_changeset(d_change(), list()) -> {ok, d_changeset()} | {error, term()}.
add_to_changeset(Change, DepIds) ->
    deliv_change:set_superseded_changes(Change),
    ChangeId = deliv_change:getval(id, Change),
    case deliv_db:qfetch(?MODULE, add_to_changeset, [ChangeId, DepIds]) of
        [Changeset] ->
            {ok, Changeset};
        {error, _} = Error ->
            Error
    end.

%% looks up each consumer for a pipeline and finds the change id for each of those pipelines
-spec consumer_change_ids_for_pipeline(non_neg_integer()) -> {ok, [binary()]} | {error, term()}.
consumer_change_ids_for_pipeline(PipelineId) ->
   case deliv_db:select(?MODULE, consumer_change_ids_by_pipeline, [PipelineId]) of
       {ok, [[{<<"consumer_change_ids">>, ChangeIds}]]} -> {ok, ChangeIds};
        {error, Why} ->
            TranslatedWhy = deliv_pg:translate_error(Why),
            chef_log:failed_call(?MODULE, consumer_change_ids_for_pipeline,
                                  [PipelineId], TranslatedWhy),
            {error, TranslatedWhy}
    end.

%% Returns a list of pipeline ids.
-spec get_dependencies_for_project(binary(), binary(), binary()) -> {ok, list()} | {error, term()}.
get_dependencies_for_project(EntName, OrgName, ProjName) ->
    case deliv_db:select(?MODULE, dependencies_by_project, [EntName, OrgName, ProjName],
                         rows_as_records,
                         [dependency_summary, record_info(fields, dependency_summary)]) of
        {ok, DepSums} ->
            {ok, DepSums};
        {error, Why} ->
            TranslatedWhy = deliv_pg:translate_error(Why),
            chef_log:failed_call(?MODULE, get_dependencies_for_project,
                                  [EntName, OrgName, ProjName], TranslatedWhy),
            {error, TranslatedWhy}
    end.

-spec dependency_ejson_for_project(binary(), binary(), binary()) -> {ok, json()} | {error, term()}.
dependency_ejson_for_project(EntName, OrgName, ProjName) ->
    case dependencies_to_ejson(get_dependencies_for_project(EntName, OrgName, ProjName)) of
        {ok, DepEjson, ReqEjson} ->
            {ok, {[
                  {<<"dependencies">>, {DepEjson}},
                  {<<"required_by">>, {ReqEjson}}
                 ]}
            };
        {error, _} = Error -> Error
    end.

%% @private
%% Take the PipeIDs from the Database and convert them to ejson
dependencies_to_ejson({ok, DepSums}) ->
    lists:foldl(fun(DepSum, Acc) -> dependency_to_ejson(DepSum, Acc) end,
                {ok, [], []}, DepSums);
dependencies_to_ejson({error, _} = Error) -> Error.

dependency_to_ejson(_, {error, _} = Error) ->
    Error;
dependency_to_ejson(#dependency_summary{pipeline_id = PipeId, dependencies = Deps,
                        consumers = Cons}, {ok, DepEjsonList, ConEjsonList}) ->
    {ok, Pipeline} = deliv_pipeline:fetch_by_id(PipeId),
    PipeName = deliv_pipeline:getval(name, Pipeline),
    case {ids_to_ejson(Deps, []), ids_to_ejson(Cons, [])} of
        {{ok, DepEjson}, {ok, ConEjson}} ->
            {ok, [{PipeName, DepEjson} | DepEjsonList],
             [{PipeName, ConEjson} | ConEjsonList]};
        {{error, _} = Error, _} ->
            Error;
        {_, {error, _} = Error} ->
            Error
    end.

%% @private
%% Accumulator to transform Pipeline IDs into Ejson
ids_to_ejson([], DepList) ->
    {ok, DepList};
ids_to_ejson([PipeId | RemPipeIds], DepList) ->
    case pipeline_to_ejson(deliv_pipeline:fetch_by_id(PipeId)) of
        {error, _} = Error -> Error;
        Dep -> ids_to_ejson(RemPipeIds, [Dep | DepList])
    end.

%% @private
%% For the given pipeline, create a ejson blob to add to the response for the
%% `ids_to_json` accumulator
pipeline_to_ejson({ok, Pipeline}) ->
    PipeId = deliv_pipeline:getval(id, Pipeline),
    PipeName = deliv_pipeline:getval(name, Pipeline),
    case deliv_pipeline:scoping_names(PipeId) of
        [EntName, OrgName, ProjName] ->
            {[
                {<<"enterprise">>, EntName},
                {<<"organization">>, OrgName},
                {<<"project">>, ProjName},
                {<<"pipeline">>, PipeName}
            ]};
        {error, _} = Error -> Error
    end;
pipeline_to_ejson({error, _} = Error) -> Error.

%% @doc Closes the currently open changeset
-spec close_changeset(binary(), d_user()) -> ok | {error, _Why}.
close_changeset(ChangeId, User) ->
    UserName = deliv_user:getval(name, User),
    Params = [ChangeId, UserName],
    case sqerl_rec:cquery(?MODULE, close_changeset,
                          Params) of
        {ok, NumberOfChangesUpdated} ->
            publish_closed_changeset_event(ChangeId, NumberOfChangesUpdated),
            ok;
        {error, Why} = Error ->
            chef_log:error("Failed to close changeset for ~p: ~p",
                            [Params, Why]),
            Error
    end.

%% @doc Return the most recent changeset to be delivered for the given pipeline.
-spec latest_closed_for_pipeline(non_neg_integer()) -> {ok, d_changeset()} |
                                                       {ok, undefined} |
                                                       {error, term()}.
latest_closed_for_pipeline(PipeId) ->
    case deliv_db:qfetch(?MODULE, latest_closed_for_pipeline, [PipeId]) of
        [] ->
            {ok, undefined};
        [Changeset] ->
            {ok, Changeset};
        {error, Why} = Error ->
            chef_log:failed_call(?MODULE, latest_closed_for_pipeline,
                                  [PipeId], Why),
            Error
    end.

%% private
publish_closed_changeset_event(ChangeId, NumChangesUpdated) ->
    {ok, Change} = deliv_change:fetch_by_id(ChangeId),
    %% Fetch all changes delivered with this changeset.
    %% If there was only one change in this changeset (NumChangesUpdated = 1)
    %% then we have have all the changes in this changeset and there is no need
    %% to query the database.
    DeliveredChangeList = case NumChangesUpdated of
        1 ->
            [Change];
        _ -> % > 1
            ChangeSetId = deliv_change:getval(changeset_id, Change),
            deliv_change:changelog_for_changeset(ChangeSetId)
    end,
    publish_delivered_event(DeliveredChangeList).

publish_delivered_event({error, Why}) ->
    chef_log:error("Cannot publish change_delivered events. Failed to fetch changes for changeset: ~p", [Why]);
publish_delivered_event([]) ->
    ok; % nothing to do!
publish_delivered_event([Change | Rest]) ->
    deliv_event:publish(change_delivered, Change),
    publish_delivered_event(Rest).

-module(deliv_stage_run).

-include("deliv_types.hrl").

-compile({parse_transform, sqerl_gobot}).

-ifdef(TEST).
-compile(export_all).
-endif.

%% DB operations
-export([
         current_pipeline_status_for_enterprise/1,
         fetch/1,
         fetch_all_by_change_id/1,
         finish/2,
         get_stage_runs_to_restart/0,
         insert/1,
         start/1,
         update/2
        ]).

%% sqerl callbacks
-export([
         '#insert_fields'/0,
         '#update_fields'/0,
         '#statements'/0,
         '#table_name'/0
        ]).

-record(deliv_stage_run, {
          id               :: db_id(),
          change_id        :: db_guid(),
          stage            :: binary(),
          finished = false :: boolean(),
          status           :: binary(),
          created_at       :: calendar:datetime(),
          started_at       :: calendar:datetime(),
          finished_at      :: calendar:datetime()
        }).

'#insert_fields'() ->
    [change_id, stage, finished, status].

'#update_fields'() ->
    [finished, status, started_at, finished_at].

'#statements'() ->
    [default,
     {fetch_by_change_id,
      sqerl_rec:gen_fetch(?MODULE, change_id)},
     {stage_runs_to_restart,
      <<"SELECT * FROM stage_runs_to_restart">>},
     %% $1 = enterprise name (binary)
     {current_pipeline_status_for_enterprise,
      <<"SELECT id, title, org, project, stage, stage_status,
         submitter, submitted_at, approved_by, delivered_by,
         changeset_id
         FROM current_pipeline_status_for_enterprise($1)">>},
     {changes_for_changeset_ids,
      <<"SELECT *
         FROM changes_for_changeset_ids($1)">>}
    ].

'#table_name'() ->
    "stage_runs".

-spec insert(d_stage_run() | proplist(atom(), any()))
        -> db_op_result(d_stage_run()).
insert(#deliv_stage_run{} = Rec) ->
    deliv_db:insert(Rec);
insert(PropList) when erlang:is_list(PropList)->
    insert(fromlist(PropList)).

-spec fetch(non_neg_integer()) -> {ok, d_stage_run()} | {error, _}.
fetch(StageRunId) ->
    deliv_db:fetch_by_id(?MODULE, StageRunId).

%% @doc Fetch all stage runs associated with `ChangeId'
-spec fetch_all_by_change_id(binary()) -> db_op_result(d_stage_run()).
fetch_all_by_change_id(ChangeId) ->
    deliv_db:qfetch(?MODULE, fetch_by_change_id, [ChangeId]).

%% @doc Fetch all stage runs associated in `Status'
-spec get_stage_runs_to_restart() -> db_op_result(d_stage_run()).
get_stage_runs_to_restart() ->
    deliv_db:qfetch(?MODULE, stage_runs_to_restart, []).

%% When a record with a timestamp is updated, the timestamp needs to be
%% converted to a string because the underlying database adapter (sqerl)
%% can't serialize timestamps on its own and raises an exception.
-spec update(proplist(atom(), any()), d_stage_run())
    -> {ok, d_stage_run()} | {error, _}.
update(PropList, #deliv_stage_run{started_at = StartedAt, finished_at = FinishedAt} = StageRun) ->
    StageRunWithSerializedTimestamps = StageRun#deliv_stage_run{
      started_at = serialized_timestamp_or_undefined(StartedAt),
      finished_at = serialized_timestamp_or_undefined(FinishedAt)
    },
    deliv_db:update(PropList, StageRunWithSerializedTimestamps).

-spec serialized_timestamp_or_undefined(undefined | calendar:datetime())
    -> undefined | binary().
serialized_timestamp_or_undefined(undefined) ->
    undefined;
serialized_timestamp_or_undefined(Timestamp) ->
    chef_utils:format_db_timestamp(Timestamp).

%% @doc First pass at pipeline status for front-end visualization.
%%
%% Return a list of changes that are currently going through the pipeline.
%% More specifically:
%%  * the 0-3 latest changes in URD
%%  * for each pipeline, the latest undelivered changes for build and acceptance
%%  * all the unmerged changes in verify
%%
%% The idea here is a kind of "Unix Top" for Delivery. If there are changes in any of
%% the stages/states above, list them, but if not, don't list anything.
%%
-spec current_pipeline_status_for_enterprise(binary()) -> {ok, ej:json_object()} |
                                                          {error, term()}.
current_pipeline_status_for_enterprise(EntName) ->
    case deliv_db:select(?MODULE, current_pipeline_status_for_enterprise, [EntName]) of
        {ok, Rows} ->
            ChangeSets = unique_changesets_for_stage(Rows),
            PipelineChanges = deliv_db:select(?MODULE, changes_for_changeset_ids, [ChangeSets]),
            transform_to_ejson(Rows, PipelineChanges);
        {error, _} = Error ->
            Error
    end.

unique_changesets_for_stage(Rows) ->
    unique_changesets_for_stage(Rows, []).

unique_changesets_for_stage([], Acc) ->
    lists:usort(lists:reverse(Acc));
unique_changesets_for_stage([H | T], Acc) ->
    case proplists:get_value(<<"changeset_id">>, H) of
        undefined ->
            unique_changesets_for_stage(T, Acc);
        ChangeSetId ->
            unique_changesets_for_stage(T, [ChangeSetId | Acc])
    end.

transform_to_ejson(_, {error, _} = Error) ->
    Error;
transform_to_ejson(Rows, {ok, IncludedChanges}) ->
    {ok, deliv_pipeline_status_json:create_body(Rows, IncludedChanges)}.

-spec start(d_stage_run())
        -> {ok, d_stage_run()} | {error, _}.
start(StageRun) ->
    PropList = [{status, <<"running">>},
                {started_at, chef_utils:db_now()}],
    update(PropList, StageRun).

-spec finish(d_stage_run(), failed | passed)
        -> {ok, d_stage_run()} | {error, _}.
finish(StageRun, failed) ->
    PropList = [{finished, true},
                {finished_at, chef_utils:db_now()}],
    update(PropList, StageRun);
finish(StageRun, passed) ->
    PropList = [{status, <<"passed">>},
                {finished, true},
                {finished_at, chef_utils:db_now()}],
    update(PropList, StageRun).

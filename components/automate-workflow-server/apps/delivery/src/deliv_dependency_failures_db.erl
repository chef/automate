-module(deliv_dependency_failures_db).

%% Provides CRD functionality for dependency failures data

-behaviour(deliv_sqerl_rec).

-include("deliv_types.hrl").

-export([
         fetch/1,
         insert/2
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

-record(deliv_dependency_failures_db, {
          enterprise_id :: db_id(),
          grouping_id   :: non_neg_integer(),
          pipeline_id   :: db_id(),
          status        :: binary()
         }).
'#insert_fields'() ->
    [].

'#update_fields'() ->
    [].

'#statements'() ->
    [
     default,
      {insert_by_name,
      <<"WITH ents AS (
          SELECT id
          FROM enterprises
          where name = $1)
          INSERT into dependency_failures (
          enterprise_id,
          grouping_id,
          pipeline_id,
          status
          )
          VALUES ((SELECT id FROM ents), $2, $3, $4)">>},
      {fetch_by_name,
      <<"WITH ents AS (
          SELECT id
          FROM enterprises
          where name = $1)
          SELECT $1, grouping_id, pipeline_id, status
          FROM dependency_failures
          WHERE enterprise_id = (SELECT id FROM ents)">>},
      {delete_by_name,
      <<"WITH ents AS (
          SELECT id
          FROM enterprises
          WHERE name = $1)
          DELETE FROM dependency_failures
          WHERE enterprise_id = (SELECT id FROM ents)">>}
    ].

'#table_name'() ->
    "dependency_failures".

-compile({parse_transform, sqerl_gobot}).

-spec scoping_parent() -> {atom(), atom()}.
scoping_parent() ->
    {deliv_enterprise, enterprise_id}.

-spec scoping_column_name() -> name.
scoping_column_name() -> name.

-spec scoping_index_constraint() -> none.
scoping_index_constraint() -> none.

-spec fetch(binary()) -> {ok, [pipeline_statuses()]} | {error, _}.
fetch(EntName) ->
    % Ensure the enterprise exists before we fetch so we can distinguish
    % between cases where an enterprise doesn't exist on the server and the
    % enterprise has no dependency failures.
    case deliv_enterprise:fetch(EntName) of
        {ok, _} ->
            {ok, deserialize(sqerl_rec:qfetch(?MODULE, fetch_by_name, [EntName]))};
        {error, _Why} = Error ->
            Error
    end.

-spec insert(binary(), [pipeline_statuses()]) -> [tuple()] | {error, _}.
insert(EntName,[]) ->
  sqerl_rec:cquery(?MODULE, delete_by_name, [EntName]),
  [];
insert(EntName,DependencyFailures) ->
  sqerl_rec:cquery(?MODULE, delete_by_name, [EntName]),
  insert(EntName, DependencyFailures, 0).

insert(EntName, [DependencyFailure | []], GroupingId) ->
  insert_dependency_failure(EntName, maps:to_list(DependencyFailure), GroupingId);
insert(EntName, [DependencyFailure | Remaining], GroupingId) ->
  insert_dependency_failure(EntName, maps:to_list(DependencyFailure), GroupingId),
  insert(EntName, Remaining, GroupingId+1).

insert_dependency_failure(EntName, [{PipelineId, Status} | []], GroupingId) ->
    sqerl_rec:qfetch(?MODULE, insert_by_name, [EntName, GroupingId, PipelineId, Status]);
insert_dependency_failure(EntName, [{PipelineId, Status} | Rest], GroupingId) ->
    sqerl_rec:qfetch(?MODULE, insert_by_name, [EntName, GroupingId, PipelineId, Status]),
    insert_dependency_failure(EntName, Rest, GroupingId).

deserialize([] = Records) ->
  Records;
deserialize([#deliv_dependency_failures_db{grouping_id = GroupingId} | _] = Records) ->
  deserialize(Records, GroupingId, []).

deserialize([], _, Acc) ->
  lists:reverse(Acc);
deserialize([#deliv_dependency_failures_db{grouping_id = FailureGroupingId, pipeline_id = PipelineId, status = Status } | Rest], GroupingId, Acc) ->
  {NextGroupingId, NextAcc} = determine_next_grouping_id(FailureGroupingId, GroupingId, Acc),
  UpdatedAcc = update_accumulator(PipelineId, Status, NextAcc),
  deserialize(Rest, NextGroupingId, UpdatedAcc).

% Just Starting
determine_next_grouping_id(GroupingId, _, []) ->
  {GroupingId, [#{}]};
% Found a new GroupingId
determine_next_grouping_id(A, B, Acc) when A /= B ->
  {A, [#{} | Acc]};
% Repetition in the same GroupingId
determine_next_grouping_id(A, A, Acc) ->
  {A, Acc}.

update_accumulator(PipelineId, Status, [Map | Rest]) ->
  UpdatedMap = maps:put(PipelineId, erlang:binary_to_existing_atom(Status, utf8), Map),
  [UpdatedMap | Rest].

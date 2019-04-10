-module(deliv_pipeline_status_json).

-export([
         create_body/2
        ]).

%% @doc Takes two lists of proplists.
%% The second list of proplists will be added to the first
%% lists `include` field joined by changeset_id.
%%
%% Returns a list of ejson.
create_body(ChangeRows, IncludedChangeRows) ->
    MergedDataSet = [ add_included_changes(RowProplist, IncludedChangeRows) || RowProplist <- ChangeRows],
    [create_body(ChangePropList) || ChangePropList <- MergedDataSet].

create_body(PropList) ->
    {EjsonProplist} = create_ejson_body(PropList),
    IncludedChanges = proplists:get_value(<<"includes">>, PropList),
    IncludedChangesEjson = [ create_ejson_body(ChangePropList) || ChangePropList <- IncludedChanges],
    {EjsonProplist ++ [{<<"includes">>, IncludedChangesEjson}]}.

add_included_changes(PropList, IncludedChanges) ->
    ChangesetId = proplists:get_value(<<"changeset_id">>, PropList),
    IncludedChangesForChangesetId = lists:filter(fun(IncludedChange) ->
                                                ChangesetId =:= proplists:get_value(<<"changeset_id">>, IncludedChange) end,
                                                IncludedChanges),
    [{<<"includes">>, IncludedChangesForChangesetId}] ++ PropList.

create_ejson_body(PropList) ->
    Id          = required_field(<<"id">>, PropList),
    Title       = required_field(<<"title">>, PropList),
    Org         = required_field(<<"org">>, PropList),
    Project     = required_field(<<"project">>, PropList),
    Stage       = required_field(<<"stage">>, PropList),
    StageStatus = required_field(<<"stage_status">>, PropList),
    Submitter   = proplists:get_value(<<"submitter">>, PropList),
    SubmittedAt = parse_time_if_present(proplists:get_value(<<"submitted_at">>, PropList)),
    ApprovedBy  = proplists:get_value(<<"approved_by">>, PropList),
    DeliveredBy = proplists:get_value(<<"delivered_by">>, PropList),
    {[{<<"id">>, Id},
      {<<"title">>, Title},
      {<<"org">>, Org},
      {<<"project">>, Project},
      {<<"stage">>, Stage},
      {<<"stage_status">>, StageStatus},
      {<<"submitter">>, Submitter},
      {<<"submitted_at">>, SubmittedAt},
      {<<"approved_by">>, ApprovedBy},
      {<<"delivered_by">>, DeliveredBy}
]}.

%% This timestamp can come in as either `null' (if NULL in the DB)
%% or `undefined' (if not present in the prop list)
parse_time_if_present(null) ->
  undefined;
parse_time_if_present(undefined) ->
  undefined;
parse_time_if_present({{_,_,_},{_,_,_}} = Timestamp) ->
  chef_utils:format_timestamp(Timestamp);
parse_time_if_present(BadArg) ->
  erlang:error({badarg, {invalid_timestamp, BadArg}}).

required_field(Key, PropList) ->
  case proplists:get_value(Key, PropList) of
    undefined ->
      erlang:error({badarg, {missing_required_field, Key}});
    Value ->
      Value
  end.

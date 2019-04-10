-module(jobs_job_json).

-include("jobs_types.hrl").

-export([
         to_json/1
        ]).

-spec to_json([job()] | job()) -> json().
to_json(Jobs) when is_list(Jobs) ->
    lists:map(fun to_json/1, Jobs);
to_json(#job{
           started_running_at = undefined,
           deliv_change_info = #deliv_change_info{submitted_at = SubmittedAt}
          } = Job) ->
    to_json(SubmittedAt, Job);
to_json(#job{started_running_at = StartedRunningAt} = Job) ->
    to_json(StartedRunningAt, Job).

to_json(TimeToDelta, #job{
                        id = Id,
                        status = Status,
                        deliv_change_info = #deliv_change_info{
                                               id = ChangeId,
                                               title = Change,
                                               org = OrgName,
                                               stage = Stage,
                                               phase = Phase,
                                               project = Project,
                                               submitted_at = SubmittedAt
                                              }}) ->

    DeltaFormatted = format_seconds(difference_in_seconds(chef_utils:trunc_timestamp(TimeToDelta), calendar:universal_time())),

    {[
      {<<"id">>, Id},
      {<<"project">>, Project},
      {<<"change">>, {[
                       {<<"id">>, ChangeId},
                       {<<"title">>, Change}]}},
      {<<"org">>, OrgName},
      {<<"stage">>, Stage},
      {<<"phase">>, Phase},
      {<<"status">>, Status},
      {<<"submittedAt">>, chef_utils:format_timestamp(SubmittedAt)},
      {<<"timeInState">>, DeltaFormatted}
     ]}.

-spec difference_in_seconds(calendar:datetime(), calendar:datetime()) -> non_neg_integer().
difference_in_seconds(Earlier, Later) ->
    calendar:datetime_to_gregorian_seconds(Later) - calendar:datetime_to_gregorian_seconds(Earlier).

-spec format_seconds(non_neg_integer()) -> binary().
format_seconds(Delta) ->
    format_daystime(calendar:seconds_to_daystime(Delta)).

-spec format_daystime({integer(), calendar:time()}) -> binary().
format_daystime({0, {Hours, Minutes, Seconds}}) ->
    chef_utils:to_bin("~2..0B:~2..0B:~2..0B", [Hours, Minutes, Seconds]);
format_daystime({Days, {Hours, Minutes, Seconds}}) ->
    chef_utils:to_bin("~Bd ~2..0B:~2..0B:~2..0B", [Days, Hours, Minutes, Seconds]).

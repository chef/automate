-module(audit_web_utils).

-export([
         to_ejson/1,
         to_json/1
        ]).

-include("audit_events.hrl").

-ifdef(TEST).
-compile(export_all).
-endif.

-spec to_json([#audit_stage_event{}]) -> binary().
to_json(ListOfEvents) ->
    chef_json:encode(to_ejson(ListOfEvents)).

to_ejson(ListOfEvents) ->
    to_ejson(ListOfEvents, []).

to_ejson([H | T], Acc) ->
    to_ejson(T, [convert_event_to_ejson(H) | Acc]);
to_ejson([], Acc) ->
    lists:reverse(Acc).

convert_event_to_ejson(#audit_stage_event{
                          change_id = ChangeId,
                          change_title = ChangeTitle,
                          ent = Ent,
                          org = Org,
                          pipe = Pipe,
                          proj = Proj,
                          stage_name = StageName,
                          action = Action,
                          status = Status,
                          create_time = CreateTime,
                          submitted_at = StartedAt,
                          submitted_by = StartedBy,
                          approved_by = ApprovedBy,
                          delivered_by = DeliveredBy}) ->
    {[
      {<<"change_id">>,         ChangeId},
      {<<"change_title">>,      ChangeTitle},
      {<<"status">>,            Status},
      {<<"ent">>,               Ent},
      {<<"org">>,               Org},
      {<<"pipe">>,              Pipe},
      {<<"proj">>,              Proj},
      {<<"stage_name">>,        StageName},
      {<<"action">>,            Action},
      {<<"create_time">>,       chef_utils:format_timestamp(CreateTime)},
      {<<"submitted_at">>,      chef_utils:format_timestamp(StartedAt)},
      {<<"submitted_by">>,      StartedBy},
      {<<"approved_by">>,       ApprovedBy},
      {<<"delivered_by">>,      DeliveredBy}
     ]}.

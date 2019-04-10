-module(audit_stage_event_db).

-include("audit_events.hrl").

-compile({parse_transform, sqerl_gobot}).

-record(audit_stage_event_db, {
     id,
     action,
     status,
     create_time,
     change_id,
     change_title,
     ent,
     org,
     pipe,
     proj,
     stage_name,
     submitted_at,
     submitted_by,
     approved_by,
     delivered_by
    }).
%% sqerl callbacks
-export([
     '#insert_fields'/0,
     '#update_fields'/0,
     '#statements'/0,
     '#table_name'/0
    ]).

-export([
     insert/1,
     fetch_audit_events/1
    ]).

'#insert_fields'() -> [].

'#update_fields'() -> [].

'#statements'() ->
 [
  default,
  {insert,
   <<"WITH ents AS (
       SELECT id
       FROM enterprises
       where name = $6)
       INSERT into audit_stage_events (
       id,
       action,
       create_time,
       status,
       change_id,
       change_title,
       ent_id,
       ent,
       org,
       pipe,
       proj,
       stage_name,
       submitted_at,
       submitted_by,
       approved_by,
       delivered_by
       )
       VALUES (uuid_generate_v4(), $1, $2, $3, $4, $5,
       (select id from ents),
       $6, $7, $8, $9, $10, $11,
       $12, $13, $14)">>},
  {fetch_audit_events,
   <<"SELECT * FROM audit_stage_events
       ORDER BY order_by DESC
       LIMIT $1">>}
 ].

'#table_name'() -> "audit_stage_events".

 insert(AuditEvent) ->
   Params = serialize_params(AuditEvent),
   case sqerl_rec:cquery(?MODULE, insert, Params) of
     {ok, AuditDbRecord} ->
       AuditDbRecord;
     Other ->
       Other
   end.

serialize_params(
  #audit_stage_event{
     action = Action,
     create_time = CreateTime,
     status = Status,
     change_id = ChangeId,
     change_title = ChangeTitle,
     ent = Ent,
     org = Org,
     pipe = Pipe,
     proj = Proj,
     stage_name = StageName,
     submitted_at = SubmittedAt,
     submitted_by = SubmittedBy,
     approved_by = ApprovedBy,
     delivered_by = DeliveredBy
    }
 )->
  [
   Action,
   chef_utils:format_db_timestamp(CreateTime),
   Status,
   ChangeId,
   ChangeTitle,
   Ent,
   Org,
   Pipe,
   Proj,
   StageName,
   chef_utils:format_db_timestamp(SubmittedAt),
   SubmittedBy,
   ApprovedBy,
   DeliveredBy
  ].
deserialize(Records) when is_list(Records) ->
  [deserialize(Record) || Record <- Records];
deserialize(#audit_stage_event_db{
               action = Action,
               create_time = CreateTime,
               status = Status,
               change_id = ChangeId,
               change_title = ChangeTitle,
               ent = Ent,
               org = Org,
               pipe = Pipe,
               proj = Proj,
               stage_name = StageName,
               submitted_at = SubmittedAt,
               submitted_by = SubmittedBy,
               approved_by = ApprovedBy,
               delivered_by = DeliveredBy
              }) ->
  #audit_stage_event{
     action = binary_to_existing_atom(Action, utf8),
     create_time = chef_utils:trunc_timestamp(CreateTime),
     status = Status,
     change_id = ChangeId,
     change_title = ChangeTitle,
     ent = Ent,
     org = Org,
     pipe = Pipe,
     proj = Proj,
     stage_name = binary_to_existing_atom(StageName, utf8),
     submitted_at = chef_utils:trunc_timestamp(SubmittedAt),
     submitted_by = SubmittedBy,
     approved_by = ApprovedBy,
     delivered_by = DeliveredBy
    }.

fetch_audit_events(MaxCount) ->
  deserialize(sqerl_rec:qfetch(?MODULE, fetch_audit_events, [MaxCount])).

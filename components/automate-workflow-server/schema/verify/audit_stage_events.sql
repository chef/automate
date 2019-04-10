-- Verify audit_stage_events

BEGIN;

 SELECT
    id,
    action,
    status,
    create_time,
    change_id,
    change_title,
    ent_id,
    ent,
    org,
    pipe,
    proj,
    stage_name,
    submitted_by,
    submitted_at,
    approved_by,
    delivered_by
 FROM audit_stage_events
 WHERE FALSE;

ROLLBACK;

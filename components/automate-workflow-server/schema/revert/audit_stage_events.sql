-- Revert audit_stage_events

BEGIN;

 DROP TABLE audit_stage_events;

COMMIT;

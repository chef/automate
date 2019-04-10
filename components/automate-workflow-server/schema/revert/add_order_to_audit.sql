-- Revert add_order_to_audit

BEGIN;

  ALTER TABLE audit_stage_events DROP COLUMN order_by;

COMMIT;

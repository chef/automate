-- Deploy add_order_to_audit

BEGIN;

  ALTER TABLE audit_stage_events ADD COLUMN order_by BIGSERIAL;

COMMIT;

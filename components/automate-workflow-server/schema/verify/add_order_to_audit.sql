-- Verify add_order_to_audit

BEGIN;

SELECT order_by
  FROM audit_stage_events
 WHERE FALSE;

ROLLBACK;

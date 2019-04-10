-- Verify delivery:add_approved_at_to_change on pg

BEGIN;

SELECT approved_at
  FROM changes
 WHERE FALSE;

ROLLBACK;

-- Verify add_approver_to_change

BEGIN;

SELECT approved_by
  FROM changes
 WHERE FALSE;

ROLLBACK;

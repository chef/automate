-- Verify add_patchset_info_to_changes

BEGIN;

SELECT latest_patchset_status
  FROM changes
 WHERE FALSE;


SELECT latest_patchset
  FROM changes
 WHERE FALSE;


SELECT submitted_at
  FROM changes
 WHERE FALSE;

SELECT submitted_by
  FROM changes
 WHERE FALSE;

ROLLBACK;

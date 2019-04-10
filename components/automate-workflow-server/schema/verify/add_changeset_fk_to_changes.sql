-- Verify add_changeset_fk_to_changes

BEGIN;

SELECT changeset_id
  FROM changes
 WHERE FALSE;

ROLLBACK;

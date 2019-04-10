-- Verify delivery:add_superseding_change_id_to_change on pg

BEGIN;

SELECT superseding_change_id
FROM changes
WHERE FALSE;

ROLLBACK;

-- Revert delivery:set_superseded_changes from pg

BEGIN;

DROP FUNCTION IF EXISTS set_superseded_changes(p_superseding_change_id changes.id%TYPE);

COMMIT;

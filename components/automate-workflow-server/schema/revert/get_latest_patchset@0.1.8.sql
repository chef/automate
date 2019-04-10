-- Revert get_latest_patchset

BEGIN;

DROP FUNCTION IF EXISTS get_latest_patchset(p_change_id changes.id%TYPE);

COMMIT;

-- Revert get_patchsets_for_change

BEGIN;

DROP FUNCTION IF EXISTS get_patchsets_for_change(p_change_id changes.id%TYPE);

COMMIT;

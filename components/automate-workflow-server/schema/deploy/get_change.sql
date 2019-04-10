-- Deploy get_change

BEGIN;

DROP FUNCTION IF EXISTS get_change(p_change_id changes.id%TYPE);

COMMIT;

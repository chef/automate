-- Revert add_to_changeset

BEGIN;

DROP FUNCTION IF EXISTS add_to_changeset(changes.id%TYPE);

COMMIT;

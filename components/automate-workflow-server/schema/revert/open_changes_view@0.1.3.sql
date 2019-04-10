-- Revert open_changes_view

BEGIN;

DROP VIEW IF EXISTS open_changes;

COMMIT;

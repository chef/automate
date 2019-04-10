-- Revert delivery:changes_for_changeset_ids from pg

BEGIN;

DROP FUNCTION IF EXISTS changes_for_changeset_ids(UUID[]);

COMMIT;

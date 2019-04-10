-- Deploy delivery:migrate_null_dependencies to pg
-- requires: add_dependencies_to_changesets

BEGIN;

UPDATE changesets
  SET dependencies = '{}'
  WHERE dependencies IS NULL;

ALTER TABLE changesets
  ALTER COLUMN dependencies
  SET NOT NULL;

ALTER TABLE changesets
  ALTER COLUMN dependencies
  SET DEFAULT '{}';

COMMIT;

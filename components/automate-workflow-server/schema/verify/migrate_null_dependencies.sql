-- Verify delivery:migrate_null_dependencies on pg

BEGIN;

-- Because there is no way to revert the migration, it does no good to
-- verify that it was successful.

ROLLBACK;

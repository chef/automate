-- Revert delivery:migrate_null_dependencies from pg

BEGIN;

-- Because there is no way to reverse the migration, we do not have
-- revert instructions.

COMMIT;

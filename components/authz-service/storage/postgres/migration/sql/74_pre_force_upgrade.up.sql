BEGIN;

-- we are slightly changing the purpose of migration_status for force upgrade.
-- we want to be able to go from init to failed now.
DROP TRIGGER migration_trigger_func ON migration_status;

DROP FUNCTION migration_trigger_func();

DROP FUNCTION valid_migration_state_change(migration_state, migration_state);

COMMIT;
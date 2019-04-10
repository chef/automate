BEGIN;

DROP TRIGGER IF EXISTS only_allow_delete_on_deletable_policies ON policies;
DROP FUNCTION IF EXISTS cannot_delete_policy_error();
ALTER TABLE policies DROP COLUMN deletable;

COMMIT;

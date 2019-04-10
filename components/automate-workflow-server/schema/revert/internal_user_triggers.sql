-- Revert internal_user_triggers

BEGIN;


DROP TRIGGER IF EXISTS insert_internal_user
  ON internal_users;

DROP TRIGGER IF EXISTS update_internal_user
  ON internal_users;

DROP TRIGGER IF EXISTS delete_internal_user
  ON internal_users;

COMMIT;

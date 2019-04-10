BEGIN;

ALTER TABLE teams_users_associations ALTER COLUMN user_id TYPE TEXT;

COMMIT;
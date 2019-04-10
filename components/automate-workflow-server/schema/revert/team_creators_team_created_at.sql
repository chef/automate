-- Revert delivery:team_creators_team_created_at from pg

BEGIN;

ALTER TABLE teams DROP CONSTRAINT IF EXISTS teams_creator_id_user_id_fkey;
ALTER TABLE teams DROP CONSTRAINT IF EXISTS teams_updater_id_user_id_fkey;
ALTER TABLE teams DROP COLUMN creator_id;
ALTER TABLE teams DROP COLUMN created_at;
ALTER TABLE teams DROP COLUMN updater_id;
ALTER TABLE teams DROP COLUMN updated_at;

COMMIT;

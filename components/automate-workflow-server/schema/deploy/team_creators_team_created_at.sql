-- Deploy delivery:team_creators_team_created_at to pg

BEGIN;

ALTER TABLE teams ADD COLUMN creator_id BIGINT;
ALTER TABLE teams ADD COLUMN created_at cd_timestamp;
ALTER TABLE teams ADD COLUMN updater_id BIGINT;
ALTER TABLE teams ADD COLUMN updated_at cd_timestamp;
ALTER TABLE teams ADD CONSTRAINT teams_creator_id_user_id_fkey FOREIGN KEY (creator_id) REFERENCES users(id) ON DELETE CASCADE;
ALTER TABLE teams ADD CONSTRAINT teams_updater_id_user_id_fkey FOREIGN KEY (updater_id) REFERENCES users(id) ON DELETE CASCADE;

COMMIT;

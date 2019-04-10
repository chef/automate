-- Deploy delivery:add_latest_change_to_changesets to pg

BEGIN;

ALTER TABLE changesets ADD COLUMN latest_change_id UUID REFERENCES changes(id);

COMMIT;

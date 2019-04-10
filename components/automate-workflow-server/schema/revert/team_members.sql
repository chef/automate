-- Revert delivery:team_members from pg

BEGIN;

  DROP TABLE IF EXISTS team_members;

COMMIT;

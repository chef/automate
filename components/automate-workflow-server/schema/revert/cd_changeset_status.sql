-- Revert cd_changeset_status

BEGIN;

DROP TYPE IF EXISTS cd_changeset_status;

COMMIT;

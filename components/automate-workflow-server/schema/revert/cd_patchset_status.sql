-- Revert cd_patchset_status

BEGIN;

DROP TYPE IF EXISTS cd_patchset_status;

COMMIT;

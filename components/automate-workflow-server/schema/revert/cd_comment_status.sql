-- Revert cd_comment_status

BEGIN;

DROP TYPE IF EXISTS cd_comment_status;

COMMIT;

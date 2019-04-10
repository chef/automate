-- Revert cd_comment_type

BEGIN;

DROP TYPE IF EXISTS cd_comment_type;

COMMIT;

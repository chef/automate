-- Revert pipeline_user_roles

BEGIN;

DROP TABLE IF EXISTS pipeline_user_roles;

COMMIT;

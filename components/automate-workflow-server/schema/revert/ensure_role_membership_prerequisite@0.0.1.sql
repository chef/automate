-- Revert ensure_role_membership_prerequisite

BEGIN;

DROP FUNCTION IF EXISTS ensure_role_membership_prerequisite();

COMMIT;

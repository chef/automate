-- Verify enterprise_user_roles

BEGIN;

SELECT enterprise_id, user_id, role
FROM enterprise_user_roles
WHERE FALSE;

ROLLBACK;

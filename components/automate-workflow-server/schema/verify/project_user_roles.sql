-- Verify project_user_roles

BEGIN;

SELECT project_id, user_id, role
FROM project_user_roles
WHERE FALSE;

ROLLBACK;

-- Verify pipeline_user_roles

BEGIN;

SELECT pipeline_id, user_id, role
FROM pipeline_user_roles
WHERE FALSE;

ROLLBACK;

-- Verify organization_user_roles

BEGIN;

SELECT organization_id, user_id, role
FROM organization_user_roles
WHERE FALSE;

ROLLBACK;

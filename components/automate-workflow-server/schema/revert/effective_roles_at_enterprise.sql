-- Revert effective_roles_at_enterprise

BEGIN;

DROP FUNCTION IF EXISTS effective_roles_at_enterprise(
  p_enterprise_name enterprises.name%TYPE,
  p_user_name users.name%TYPE);

COMMIT;

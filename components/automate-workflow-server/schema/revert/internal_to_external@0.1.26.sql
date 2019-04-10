-- Revert internal_to_external

BEGIN;

DROP FUNCTION IF EXISTS utility.internal_to_external(
  p_enterprise_name enterprises.name%TYPE,
  p_user_name users.name%TYPE);

COMMIT;

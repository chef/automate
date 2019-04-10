-- Revert changes_in_verify

BEGIN;

DROP FUNCTION IF EXISTS changes_in_verify(
  p_enterprise_name enterprises.name%TYPE,
  p_organization_name organizations.name%TYPE
);

COMMIT;

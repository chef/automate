-- Deploy assign_roles_at_enterprise

BEGIN;

CREATE OR REPLACE FUNCTION assign_roles_at_enterprise(
  p_enterprise_name enterprises.name%TYPE,
  p_user_name users.name%TYPE,
  p_roles text[])
RETURNS VOID
LANGUAGE plpgsql
AS $$
DECLARE
  v_enterprise_id enterprises.id%TYPE;
  v_user_id users.id%TYPE;
BEGIN
  SELECT id FROM enterprises WHERE name = p_enterprise_name INTO v_enterprise_id;
  SELECT id FROM users WHERE name = p_user_name AND enterprise_id = v_enterprise_id INTO v_user_id;

  -- Remove existing roles
  DELETE FROM enterprise_user_roles AS r
  WHERE r.enterprise_id = v_enterprise_id
    AND r.user_id = v_user_id;
   -- Set new roles
  INSERT INTO enterprise_user_roles(enterprise_id, user_id, role)
  SELECT v_enterprise_id, v_user_id, grants.role::delivery_role
  FROM unnest(p_roles) AS grants(role);
END;
$$;

COMMIT;

-- Deploy effective_roles_at_enterprise

BEGIN;

CREATE OR REPLACE FUNCTION effective_roles_at_enterprise(
  p_enterprise_name enterprises.name%TYPE,
  p_user_name users.name%TYPE)
RETURNS TABLE(role delivery_role)
LANGUAGE SQL
AS $$
  SELECT r.role
  FROM enterprise_user_roles AS r
  JOIN enterprises AS e
    ON e.id = r.enterprise_id
  JOIN users AS u
    ON r.user_id = u.id
   AND e.id = u.enterprise_id
 WHERE e.name = p_enterprise_name
   AND u.name = p_user_name;
$$;

COMMIT;

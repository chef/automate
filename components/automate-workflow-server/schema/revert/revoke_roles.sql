-- Revert revoke_roles

BEGIN;

DROP FUNCTION IF EXISTS revoke_roles(
  p_enterprise_name enterprises.name%TYPE,
  p_user_name users.name%TYPE,
  p_organization_name organizations.name%TYPE,
  p_project_name projects.name%TYPE,
  p_pipeline_name pipelines.name%TYPE,
  p_roles text[]
);

COMMIT;

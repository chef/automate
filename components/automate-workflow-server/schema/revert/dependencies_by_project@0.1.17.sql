-- Revert delivery:dependencies_by_project from pg

BEGIN;

DROP FUNCTION IF EXISTS dependencies_by_project(
  p_enterprise_name enterprises.name%TYPE,
  p_organization_name organizations.name%TYPE,
  p_project_name projects.name%TYPE);

COMMIT;

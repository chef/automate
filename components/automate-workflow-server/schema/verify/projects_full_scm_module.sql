-- Verify delivery:projects_full_scm_module on pg

BEGIN;

SELECT id, organization_id, guid, name, scm_module
FROM projects
WHERE FALSE;

ROLLBACK;

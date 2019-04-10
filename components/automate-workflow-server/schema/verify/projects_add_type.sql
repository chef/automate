-- Verify delivery:projects_add_type on pg

BEGIN;

SELECT id, organization_id, guid, name, type
FROM projects
WHERE FALSE;

ROLLBACK;

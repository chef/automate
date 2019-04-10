-- Verify projects

BEGIN;

SELECT id, organization_id, guid, name
FROM projects
WHERE FALSE;

ROLLBACK;

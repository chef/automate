-- Verify pipelines

BEGIN;

SELECT id, project_id, name
FROM pipelines
WHERE FALSE;

ROLLBACK;

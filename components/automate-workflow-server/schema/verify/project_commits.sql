-- Verify project_commits

BEGIN;

SELECT id,
       project_id,
       sha,
       subject,
       body
  FROM project_commits WHERE FALSE;

ROLLBACK;

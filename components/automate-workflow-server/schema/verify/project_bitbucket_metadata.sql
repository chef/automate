-- Verify delivery:project_bitbucket_metadata on pg

BEGIN;

SELECT project_id, bitbucket_project, repo_name
FROM project_bitbucket_metadata
WHERE FALSE;

ROLLBACK;

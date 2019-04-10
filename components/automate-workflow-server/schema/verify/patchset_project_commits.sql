-- Verify patchset_project_commits

BEGIN;

SELECT patchset_id,
       project_commit_id
  FROM patchset_project_commits WHERE FALSE;

ROLLBACK;

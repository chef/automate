-- Verify patchset_commits

BEGIN;

SELECT patchset_id,
       sha,
       subject,
       body
FROM patchset_commits
WHERE FALSE;

ROLLBACK;

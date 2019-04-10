-- Verify changes

BEGIN;

SELECT id, pipeline_id, feature_branch, merge_sha
FROM changes
WHERE FALSE;

ROLLBACK;

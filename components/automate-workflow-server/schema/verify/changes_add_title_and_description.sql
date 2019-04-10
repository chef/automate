-- Verify changes_add_title_and_description

BEGIN;

SELECT id, pipeline_id, feature_branch, merge_sha, title, description
FROM changes
WHERE FALSE;

ROLLBACK;

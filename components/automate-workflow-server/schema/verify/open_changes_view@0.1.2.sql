-- Verify open_changes_view

BEGIN;

SELECT enterprise_id,
       organization_id,
       project_id,
       id,
       pipeline_id,
       feature_branch,
       merge_sha,
       title,
       description
FROM open_changes
WHERE false;

ROLLBACK;

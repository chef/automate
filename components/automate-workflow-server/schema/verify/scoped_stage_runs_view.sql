-- Verify scoped_stage_runs_view

BEGIN;

SELECT enterprise_id,
       organization_id,
       project_id,
       pipeline_id,
       id,
       change_id,
       stage,
       status,
       finished
FROM scoped_stage_runs
WHERE false;

ROLLBACK;

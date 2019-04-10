-- Deploy stage_runs_to_restart_view
-- requires: scoped_stage_runs_view

BEGIN;

CREATE OR REPLACE VIEW stage_runs_to_restart AS
  WITH scoped_stage_runs_with_latest AS (
    SELECT s.*,
           max(s.id) over (partition by s.change_id) AS latest_for_change,
           max(s.id) over (partition by s.pipeline_id, s.stage) AS latest_for_stage_in_pipeline,
           max(s.id) over (partition by s.stage) AS latest_for_stage
      FROM scoped_stage_runs AS s
  )

  SELECT s1.id, s1.change_id, s1.stage, s1.status, s1.finished
    FROM scoped_stage_runs_with_latest AS s1
   WHERE s1.stage='verify'
     AND (s1.status='running' OR s1.status='idle')
     AND s1.id = s1.latest_for_change
   UNION
  SELECT s2.id, s2.change_id, s2.stage, s2.status, s2.finished
    FROM scoped_stage_runs_with_latest AS s2
   WHERE (s2.stage='build' OR s2.stage='acceptance')
     AND (s2.status='running' OR s2.status='idle')
     AND s2.id = s2.latest_for_stage_in_pipeline
     AND s2.id = s2.latest_for_change
   UNION
  SELECT s3.id, s3.change_id, s3.stage, s3.status, s3.finished
    FROM scoped_stage_runs_with_latest AS s3
   WHERE (s3.stage='union' OR s3.stage='rehearsal' OR s3.stage='delivered')
     AND (s3.status='running' OR s3.status='idle')
     AND s3.id = s3.latest_for_stage
     AND s3.id = s3.latest_for_change;

COMMENT ON VIEW stage_runs_to_restart IS
$$Provides a view of the `stage_runs` table that includes only
the stage_runs that are able to be restarted.$$;

COMMIT;

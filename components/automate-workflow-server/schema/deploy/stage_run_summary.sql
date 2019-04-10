-- Deploy stage_run_summary

/*
This query is a bit wild do to our current data model. I have tested it on
SHD and found the performance around 20 ms. This should be acceptable for now
but it should be noted that performance will degrade as more changes are added
to the system. We expect this to be alright at least until we get the eventing
stuff in and at that time we should be able to retire this query for a better
way to answer the same question. If we do hit bottle necks before then we have
a couple of options:
1. add pipline to stage_run_summary this would eliminate the join and then we
   could do this with subqueries.
2. add a trigger that fires when stage_runs are updated and store this data in
   an additional table.
*/
BEGIN;

CREATE OR REPLACE FUNCTION stage_run_summary(p_change_id changes.id%TYPE)
RETURNS TABLE(
        id stage_runs.id%TYPE,
        stage stage_runs.stage%TYPE,
        status stage_runs.status%TYPE,
        finished stage_runs.finished%TYPE,
        pipeline_latest boolean,
        system_latest boolean)
LANGUAGE SQL STABLE
AS $$
   WITH stage_runs_with_change_latest_pipeline_latest_system_latest AS (
     SELECT s.id,
            s.change_id,
            s.stage,
            s.status,
            s.finished,
            max(s.id) OVER (PARTITION BY change_id, c.pipeline_id, stage) as m_cl_id,
            max(s.id) OVER (PARTITION BY c.pipeline_id, stage) as m_pl_id,
            max(s.id) OVER (PARTITION BY stage) AS m_sl_id
       FROM stage_runs AS s
  LEFT JOIN changes AS c
         ON c.id = s.change_id)
SELECT id,
       stage,
       status,
       finished,
       case when id = m_pl_id
            then true
            else false
       end AS pipeline_latest,
       case when id = m_sl_id
            then true
            else false
       end AS system_latest
FROM stage_runs_with_change_latest_pipeline_latest_system_latest
WHERE id = m_cl_id AND change_id = $1
ORDER BY id DESC;
$$;

COMMENT ON FUNCTION stage_run_summary(p_change_id changes.id%TYPE) IS

$$Return a summary of what stages have been run for a given change, the status
of each, a boolean indicating if the stage run is the latest for the stage on
the pipeline, and a boolean indicating if the stage run is the latest for the
stage within the whole system. For changes with multiple patchsets, only the
most recent stage run information is returned (determined by maxstage run ID).
$$;

COMMIT;

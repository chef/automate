-- Deploy phase_run_summary

BEGIN;

CREATE OR REPLACE FUNCTION phase_run_summary(p_change_id changes.id%TYPE)
RETURNS TABLE(
        stage_id stage_runs.id%TYPE,
        stage stage_runs.stage%TYPE,
        stage_status stage_runs.status%TYPE,
        phase_id phase_runs.id%TYPE,
        phase phase_runs.phase%TYPE,
        phase_status phase_runs.status%TYPE)
LANGUAGE SQL STABLE
AS $$
   WITH stage_runs_with_latest_tag AS (
     SELECT s.id AS stage_id,
            s.stage,
            s.status AS stage_status,
            p.id AS phase_id,
            p.phase,
            p.status AS phase_status,
            max(s.id) OVER (partition BY s.stage) AS latest_stage
       FROM stage_runs AS s
  LEFT JOIN phase_runs AS p
         ON s.id = p.stage_run_id
       JOIN changes AS c
         ON c.id = s.change_id
      WHERE c.id = $1),
latest_stage_runs AS (
     SELECT stage_id, stage, stage_status, phase_id, phase, phase_status
     FROM stage_runs_with_latest_tag
     WHERE latest_stage = stage_id)
SELECT lsr.*
  FROM latest_stage_runs AS lsr
  JOIN stage_ordering AS so
    ON (lsr.stage, lsr.phase) = (so.stage, so.phase)
 ORDER BY so.sequence_number;
$$;

COMMENT ON FUNCTION phase_run_summary(p_change_id changes.id%TYPE) IS

$$Return a minimal summary of what stages and phases have been run for
a given change, and the status of each. For changes with multiple
patchsets, only the most recent stage run information is returned (as
determined by maximum stage run ID).
$$;

COMMIT;

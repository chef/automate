-- Deploy current_pipeline_snapshot_for_enterprise
-- requires: scoped_stage_runs_view
-- requires: open_changes_view

BEGIN;

CREATE OR REPLACE FUNCTION current_pipeline_snapshot_for_enterprise(p_enterprise_name enterprises.name%TYPE)
RETURNS TABLE(
        stage TEXT,
        stage_status TEXT,
        counter bigint)
LANGUAGE plpgsql STABLE
ROWS 11
AS $$
DECLARE
  v_enterprise_id enterprises.id%TYPE;
BEGIN

  SELECT enterprise_id
  FROM to_ids(p_enterprise_name, NULL, NULL, NULL, NULL)
  INTO v_enterprise_id;

  -- Determine the counts for open changes in Verify, across the enterprise
  RETURN QUERY SELECT 'verify'::TEXT AS "stage", sr.status AS "stage_status", count(*) AS "counter"
  FROM open_changes AS oc
  JOIN (SELECT id, change_id, status
        FROM
          (SELECT id, change_id, status, max(id) OVER (partition BY change_id) FROM stage_runs) AS work
        WHERE id = max) AS sr
    ON oc.id = sr.change_id
  WHERE oc.enterprise_id = v_enterprise_id
  GROUP BY sr.status
  ORDER BY sr.status;

  -- Determine the counts for pipelines (remember, there can be more
  -- than one in a project) in Build, across the enterprise.
  --
  -- (This is ever-so-slightly misleading now, in that we don't
  -- currently disallow the creation of multiple pipelines, even
  -- though we don't have a way of targeting them to separate
  -- "environment triples" for Union, Rehearsal, and Delivered. If you
  -- have multiple pipelines, their counts will show up here, even
  -- though we only show one status for Union, Rehearsal, and
  -- Delivered -- see below).
  RETURN QUERY SELECT 'build'::TEXT AS "stage", status AS "stage_status", count(*) AS "counter"
  FROM (
    SELECT id, s.stage, status, max(id) over (partition by pipeline_id)
    FROM scoped_stage_runs AS s
    WHERE enterprise_id = v_enterprise_id
  ) AS work
  -- Here we want to find all the stage runs for a given pipeline that
  -- are the most recent for their pipeline but are also in the build
  -- stage, indicating that this is as far as they've gone. If id =
  -- max, but the stage is not 'build', then it either hasn't gotten
  -- to build yet, or it's gone past it.
  WHERE work.id = work.max
    AND work.stage = 'build'
  GROUP BY status
  ORDER BY status;

  -- Same thing for Acceptance environments, across the enterprise
  --
  -- (The same caveat for Build applies for Acceptance; see above.)
  RETURN QUERY SELECT 'acceptance'::TEXT AS "stage", status AS "stage_status", count(*) AS "counter"
  FROM (
    SELECT id, s.stage, status, max(id) over (partition by pipeline_id)
    FROM scoped_stage_runs AS s
    WHERE enterprise_id = v_enterprise_id
  ) AS work
  WHERE work.id = work.max
    AND work.stage = 'acceptance'
  GROUP BY status
  ORDER BY status;

  -- The current state of "the" Union environment in the enterprise
  RETURN QUERY SELECT s.stage, s.status AS "stage_status", 1::BIGINT AS "counter"
  FROM scoped_stage_runs AS s
  WHERE s.enterprise_id = v_enterprise_id
    AND s.stage = 'union'
  ORDER BY id DESC LIMIT 1;

  -- The current state of "the" Rehearsal environment in the enterprise
  RETURN QUERY SELECT s.stage, s.status AS "stage_status", 1::BIGINT AS "counter"
  FROM scoped_stage_runs AS s
  WHERE s.enterprise_id = v_enterprise_id
    AND s.stage = 'rehearsal'
  ORDER BY id DESC LIMIT 1;

  -- The current state of "the" Delivered environment in the enterprise
  RETURN QUERY SELECT s.stage, s.status AS "stage_status", 1::BIGINT AS "counter"
  FROM scoped_stage_runs AS s
  WHERE s.enterprise_id = v_enterprise_id
    AND s.stage = 'delivered'
  ORDER BY id DESC LIMIT 1;

  RETURN;

END;
$$;

COMMENT ON FUNCTION current_pipeline_snapshot_for_enterprise(p_enterprise_name enterprises.name%TYPE) IS
$$Shows the current aggregated state of all work flowing through the
pipeline for an entire enterprise.

Makes a few assumptions, namely that we only have one "environment
triple" (which is fine for now, because, hey! that's all we support).

Additionally, if some stages have not yet run (e.g., you've got a
brand new enterprise that you haven't run anything through), no data
will be returned; we only return data aggregating whatever data is
currently in the system.

Currently, we return the count of all stages in a given state.  For
instance, for the Verify stage, we return the count of all open
changes across the enterprise that are currently running, have most
recently failed, or have most recently succeeded.

Similar logic applies to the Build and Acceptance stages, but there,
we aggregate at the pipeline level, instead of the change level.

For Union, Rehearsal, and Delivered, we aggregate up to the enterprise
level (since there's only one "triple" to consider).

This function is provided as a way to "bootstrap" our pipeline
statistics views; eventually we'll adopt a more event-sourced
approach. This helps us to bridge from where we are now to where we
want to be.$$;

COMMIT;

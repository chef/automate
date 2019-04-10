-- Deploy current_pipelines_stats_for_enterprise
-- requires: scoped_stage_runs_view

BEGIN;

CREATE OR REPLACE FUNCTION current_pipelines_stats_for_enterprise(p_enterprise_name enterprises.name%TYPE)
RETURNS TABLE(
        id bigint,
        org_name TEXT,
        proj_name TEXT,
        name TEXT,
        build_count bigint,
        build_status TEXT,
        acceptance_count bigint,
        acceptance_status TEXT,
        last_deployed cd_timestamp,
        last_delivered cd_timestamp)
LANGUAGE plpgsql STABLE
AS $$
DECLARE
  v_enterprise_id enterprises.id%TYPE;
BEGIN
  SELECT enterprise_id
  FROM to_ids(p_enterprise_name, NULL, NULL, NULL, NULL)
  INTO v_enterprise_id;

  RETURN QUERY
  WITH pipelines_for_enterprise AS (
    SELECT pipe.id, org.name AS org_name, proj.name AS proj_name, pipe.name
      FROM pipelines AS pipe
      JOIN projects AS proj ON proj.id=pipe.project_id
      JOIN organizations AS org ON org.id=proj.organization_id
     WHERE org.enterprise_id = v_enterprise_id
  ),

  scoped_stage_runs_with_latest AS (
    SELECT s.*,
           max(s.id) over (partition by s.pipeline_id) AS latest_for_entire_pipeline,
           max(s.id) over (partition by s.pipeline_id, s.stage) AS latest_for_stage
      FROM scoped_stage_runs AS s
     WHERE s.enterprise_id = v_enterprise_id
  ),

  pipelines_build_counts AS (
    SELECT ssr.pipeline_id, COUNT(DISTINCT ssr.change_id)
      FROM scoped_stage_runs AS ssr
      JOIN scoped_stage_runs_with_latest AS s
        ON ssr.pipeline_id = s.pipeline_id
       AND s.id = s.latest_for_stage
       AND s.stage = 'acceptance'
       AND ssr.id > s.latest_for_stage
     WHERE ssr.stage = 'build'
     GROUP BY ssr.pipeline_id
  ),

  pipelines_acceptance_counts AS (
    SELECT ssr.pipeline_id, COUNT(DISTINCT ssr.change_id)
      FROM scoped_stage_runs AS ssr
      JOIN scoped_stage_runs_with_latest AS s
        ON ssr.pipeline_id = s.pipeline_id
       AND s.id = s.latest_for_stage
       AND s.stage = 'union'
       AND ssr.id > s.latest_for_stage
     WHERE ssr.stage = 'acceptance'
     GROUP BY ssr.pipeline_id
  ),

  pipelines_status AS (
    SELECT ssr.pipeline_id, ssr.status, ssr.stage
    FROM scoped_stage_runs_with_latest AS ssr
    WHERE ssr.latest_for_stage = ssr.id
  )

SELECT pfe.id,
       pfe.org_name,
       pfe.proj_name,
       pfe.name,
       COALESCE(pbs.count, 0) AS "build_count",
       COALESCE(ps_b.status, 'passed') AS "build_status",
       COALESCE(pas.count, 0) AS "acceptance_count",
       COALESCE(ps_a.status, 'passed') AS "acceptance_status",
       CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp),
       CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp)
FROM pipelines_for_enterprise AS pfe
LEFT JOIN pipelines_build_counts AS pbs ON pfe.id = pbs.pipeline_id
LEFT JOIN pipelines_acceptance_counts AS pas ON pfe.id = pas.pipeline_id
LEFT JOIN pipelines_status AS ps_b ON pfe.id = ps_b.pipeline_id
                                  AND ps_b.stage = 'build'
LEFT JOIN pipelines_status AS ps_a ON pfe.id = ps_a.pipeline_id
                                  AND ps_a.stage = 'acceptance'
ORDER BY pfe.id;

END;
$$;

COMMENT ON FUNCTION current_pipelines_stats_for_enterprise(p_enterprise_name enterprises.name%TYPE) IS
$$Returns a list of all pipelines for an enterprise with some statistics
about how many changes are building and waiting to be delivered.

Makes a few assumptions, namely that we only have one "environment
triple" (which is fine for now, because, hey! that's all we support).

It is currently hard coding the the time data for last build time and
last delivered time to now until we add those columns to the stage_run
and phase_run tables. This so the fucntion signature stays the same.

This function is provided as a way to "bootstrap" our pipelines
list view; eventually we'll adopt a more event-sourced
approach. This helps us to bridge from where we are now to where we
want to be.$$;

COMMIT;

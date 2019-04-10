-- Deploy current_pipeline_status_for_enterprise
-- requires: scoped_stage_runs_view

BEGIN;

DROP FUNCTION IF EXISTS current_pipeline_status_for_enterprise(text);
CREATE OR REPLACE FUNCTION current_pipeline_status_for_enterprise(p_enterprise_name enterprises.name%TYPE)
RETURNS TABLE(
        id UUID,
        title TEXT,
	org TEXT,
	project TEXT,
        stage TEXT,
        stage_status TEXT)
LANGUAGE plpgsql STABLE
ROWS 3
AS $$
DECLARE
  v_enterprise_id enterprises.id%TYPE;
BEGIN

  -- This function returns the latest set of changes that are in the URD, whatever
  -- their status. It does not consider previous changes, as those have been supplanted
  -- by the current set. In this way, it present a set similar to a 'Unix Top' report.

  -- Get the enterprise id by the incoming parameter enterprise name
  SELECT enterprise_id
  FROM to_ids(p_enterprise_name, NULL, NULL, NULL, NULL)
  INTO v_enterprise_id;


  -- First, partition by enterprise and stage with the max stage run ids. This gives sets
  -- of currently running URD changes grouped by enterprise and stage.
  RETURN QUERY WITH stage_runs_by_ent AS (
    SELECT DISTINCT MAX(ssr.id) over (partition by ssr.enterprise_id, ssr.stage)
        AS max_id
    FROM scoped_stage_runs AS ssr
    WHERE ssr.enterprise_id = v_enterprise_id
      AND ssr.stage IN ('union', 'rehearsal', 'delivered')
  ),

  -- Now partition by change_id
  change_status AS (
    SELECT ssr.*, MAX(ssr.id) over (partition BY ssr.change_id) AS latest_for_change
    FROM scoped_stage_runs AS ssr
    JOIN stage_runs_by_ent AS esr
      ON ssr.id = esr.max_id
  ),
  -- show the lastest status for changes in URD only
  latest_change_status AS (
    SELECT *
    FROM change_status cs
    WHERE cs.id = cs.latest_for_change
    -- The following clause will filter out changes that [delivered, passed] For now,
    -- it's not a bad thing to see changes in this state, as it shows us what is happening now.
    -- If these changes are not interesting, un-comment this clause.
    --     AND NOT (stage = 'delivered' AND status = 'passed')
 )

 -- Finally, return the changes with appropriate output columns.
 SELECT ssr.change_id,
         c.title,
	 o.name,
	 p.name,
         ssr.stage,
         ssr.status
  FROM scoped_stage_runs AS ssr
  JOIN changes AS c
    ON c.id = ssr.change_id
  JOIN latest_change_status AS lcs
    ON lcs.id = ssr.id
  JOIN organizations AS o
    ON o.id = ssr.organization_id
  JOIN projects AS p
    ON p.id = ssr.project_id
   AND p.organization_id = ssr.organization_id;

  RETURN;

END;
$$;

COMMIT;

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
        stage_status TEXT,
        submitter TEXT,
        submitted_at cd_timestamp,
        approved_by TEXT,
        delivered_by TEXT)
LANGUAGE plpgsql STABLE
AS $$
DECLARE
  v_enterprise_id enterprises.id%TYPE;
  v_urd_latest_stage_runs BIGINT[];
  v_undelivered_changes_latest_stage_runs BIGINT[];
  v_unmerged_changes_latest_stage_runs BIGINT[];
  v_oldest_closed_change cd_timestamp;
  v_error_changes_id changes.id%TYPE;
BEGIN
  -- Get the enterprise id by the incoming parameter enterprise name
  SELECT enterprise_id
  FROM to_ids(p_enterprise_name, NULL, NULL, NULL, NULL)
  INTO v_enterprise_id;

  -- Get the URD's latest stage runs
  SELECT ARRAY(
    SELECT MAX(ssr.id)
      FROM scoped_stage_runs AS ssr
     WHERE ssr.enterprise_id = v_enterprise_id
       AND ssr.stage IN ('union', 'rehearsal', 'delivered')
  GROUP BY ssr.stage
  ) INTO v_urd_latest_stage_runs;

  -- Then the latest stage runs for each pipeline's acceptance stage
  -- only for those which haven't been delivered yet
  SELECT ARRAY(
    SELECT MAX(ssr.id)
      FROM scoped_stage_runs AS ssr
      JOIN changes AS c
        ON c.id = ssr.change_id
      JOIN changesets AS chs
        ON chs.id = c.changeset_id
     WHERE ssr.enterprise_id = v_enterprise_id
       AND chs.status = 'open'
       AND ssr.stage = 'acceptance'
  GROUP BY ssr.stage, ssr.pipeline_id
  ) INTO v_undelivered_changes_latest_stage_runs;

  -- find the oldest change in the oldest changeset
  -- use its submitted_at which is always the submitted_at
  -- of the oldest patchset in the change as a filter
  -- for any changes that occurred prior to changesets
  -- being implemented.
  SELECT c.submitted_at
        FROM changesets AS chs,
             changes AS c
        WHERE chs.id = c.changeset_id
        ORDER BY chs.delivered_at ASC LIMIT 1
        INTO v_oldest_closed_change;
  IF NOT FOUND THEN
  -- Some systems will be born with good or no data. They should
  -- still work. We need default the oldest_changeset_change
  -- to a value other than null in the event it isn't found.
    v_oldest_closed_change = '1970-01-01 00:00:00+00';
  END IF;

  -- And finally, the latest stage runs for any change in verify or build that hasn't
  -- been delivered yet
  SELECT ARRAY(
    SELECT MAX(ssr.id)
      FROM scoped_stage_runs AS ssr
      JOIN changes AS c
        ON c.id = ssr.change_id
        AND c.changeset_id IS NULL
     WHERE ssr.enterprise_id = v_enterprise_id
       AND ssr.stage IN ('verify', 'build')
       AND c.submitted_at > v_oldest_closed_change
  GROUP BY ssr.change_id
  ) INTO v_unmerged_changes_latest_stage_runs;

  -- Let's put everything back together...
  RETURN QUERY WITH all_stage_runs AS (
    SELECT ssr.id,
           ssr.change_id,
           ssr.status,
           ssr.stage,
           ssr.organization_id,
           ssr.project_id,
           MAX(ssr.id) OVER (PARTITION BY ssr.change_id) AS latest_for_change
      FROM scoped_stage_runs AS ssr
     WHERE ssr.id = ANY(v_urd_latest_stage_runs
                       || v_undelivered_changes_latest_stage_runs
                       || v_unmerged_changes_latest_stage_runs)
  ),
  -- Now we eliminate duplicates: we only want to show the furthest
  -- any given change made it (that is, if it's the same latest change
  -- in some pipeline's acceptance, union, and rehearsal, we only want
  -- to show that change in rehearsal)
  latest_stage_runs AS (
    SELECT asr.*
      FROM all_stage_runs AS asr
     WHERE asr.id = asr.latest_for_change
  )

  -- Finally, return the changes with appropriate output columns.
  SELECT lsr.change_id,
         c.title,
         o.name,
         p.name,
         lsr.stage,
         lsr.status,
         u.name,
         mrp.submitted_at,
         c.approved_by,
         chs.delivered_by
    FROM latest_stage_runs AS lsr
    JOIN changes AS c
      ON c.id = lsr.change_id
    LEFT JOIN changesets AS chs
      ON chs.id = c.changeset_id
    LEFT JOIN most_recent_patchsets AS mrp
      ON mrp.change_id = lsr.change_id
    LEFT JOIN users AS u
      ON u.id = mrp.submitter_id
    JOIN organizations AS o
      ON o.id = lsr.organization_id
    JOIN projects AS p
      ON p.id = lsr.project_id
     AND p.organization_id = lsr.organization_id;

  RETURN;

END;
$$;

COMMENT ON FUNCTION current_pipeline_status_for_enterprise(p_enterprise_name enterprises.name%TYPE) IS
$$This function aims to return a set of changes similar to a 'Unix
Top' report; more specifically, it returns:
  * the 0-3 latest changes in URD, whatever their status. It does not
    consider previous changes, as those have been supplanted
  * for each pipeline, the latest undelivered changes for build and
    acceptance (same idea as above, it does not consider previous changes)
  * all the unmerged changes in verify
$$;

COMMIT;

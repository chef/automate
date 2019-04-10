-- Deploy delivery:changes_for_changeset_ids to pg
-- requires: scoped_stage_runs_view

BEGIN;

CREATE OR REPLACE FUNCTION changes_for_changeset_ids(p_changeset_ids UUID[])
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
        delivered_by TEXT,
        changeset_id changesets.id%TYPE)
LANGUAGE plpgsql STABLE
AS $$
DECLARE
 v_stage_ids BIGINT[];
 v_excluded_change_ids UUID[];
 v_change_ids UUID[];
BEGIN

  SELECT ARRAY(
    SELECT MAX(ssr.id)
      FROM scoped_stage_runs AS ssr,
           changes AS c,
           changesets AS chs
     WHERE chs.id = ANY(p_changeset_ids)
       AND c.changeset_id = chs.id
       AND ssr.change_id = c.id
  GROUP BY c.changeset_id
  ) INTO v_stage_ids;

  SELECT ARRAY(
    SELECT c.id
      FROM changes AS c,
           scoped_stage_runs AS ssr
     WHERE ssr.change_id = c.id
       AND ssr.id = ANY(v_stage_ids)
    ) INTO v_excluded_change_ids;

    SELECT array_agg(c.id),
           max(ssr.id)
      INTO v_change_ids
      FROM changes AS c,
           scoped_stage_runs AS ssr
     WHERE c.id = ssr.change_id
           AND ssr.stage = 'acceptance'
           AND c.changeset_id = ANY(p_changeset_ids)
           AND c.id != ALL(v_excluded_change_ids)
  GROUP BY c.id;

  RETURN QUERY
  WITH latest_changes_for_stage AS (
        SELECT c.id AS change_id,
               max(ssr.id) AS scoped_stage_run_id
          FROM changes AS c,
               scoped_stage_runs AS ssr
         WHERE c.id = ssr.change_id
           AND ssr.stage = 'acceptance'
           AND c.changeset_id = ANY(p_changeset_ids)
           AND c.id != ALL(v_excluded_change_ids)
    GROUP BY c.id
  )
   SELECT c.id,
          c.title,
          o.name,
          p.name,
          ssr.stage,
          ssr.status,
          c.submitted_by,
          c.submitted_at,
          c.approved_by,
          c.delivered_by,
          c.changeset_id
     FROM projects AS p,
          changes AS c,
          scoped_stage_runs AS ssr,
          organizations AS o
    WHERE c.id = ssr.change_id
      AND p.id = ssr.project_id
      AND p.organization_id = o.id
      AND ssr.stage = 'acceptance'
      AND c.id in (select change_id from latest_changes_for_stage)
      AND ssr.id in (select scoped_stage_run_id from latest_changes_for_stage);

  RETURN;

END;
$$;

COMMIT;

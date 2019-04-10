-- Deploy default_changesets

BEGIN;

-- Find the oldest changeset and then find all changes with null changeset_id
-- that are older than the oldest changeset.

CREATE OR REPLACE FUNCTION create_default_changesets()
RETURNS VOID
LANGUAGE plpgsql
AS $$
DECLARE
  v_change_ids UUID[];
  v_change_id changes.id%TYPE;
  v_changeset_id changesets.id%TYPE;
  v_pipeline_id pipelines.id%TYPE;
BEGIN
  -- Find all changes with stages in acceptance
  -- that have no changeset_id.
  -- Find all changes with stages in build
  -- that have no changeset_id and have failed.
  -- Stages that are in build that passed with no changeset
  -- should ALSO either have a stage in acceptance or
  -- they are PENDING going to acceptance, in which case,
  -- they will get updated when that stage runs and
  -- add_to_changeset is called.
SELECT ARRAY(SELECT DISTINCT c.id
  FROM changes as c,
  changesets as chs,
  scoped_stage_runs as ssr
  WHERE c.changeset_id IS NULL
  AND c.submitted_at <
  (SELECT COALESCE(
    -- Find the oldest closed changeset. irrespective of pipeline
    (SELECT chs.delivered_at
      WHERE chs.status = 'closed'
      ORDER BY chs.delivered_at ASC LIMIT 1),
    -- if not, include ALL changes
    NOW()
  ))
  AND merge_sha IS NOT NULL
  AND ssr.change_id = c.id
  AND (ssr.stage = 'acceptance'
  OR (
    ssr.stage = 'build'
    AND ssr.status IN ('failed', 'passed')
  )
  OR (
    -- This bandaid exists because we have bad data that
    -- came from a bug that was squashed in february of 2015
    -- where a build stage could be stuck in running even tho
    -- it passed. It is possible that a customer could have hit
    -- this bug, which would get addressed on the next changeset
    -- they ship for that pipeline. However, if they are not using
    -- that pipeline anymore, the bug will persist forever. The
    -- bandaid prevents that problem.
    ssr.stage = 'build'
    AND ssr.status IN ('idle', 'running')
    AND c.submitted_at <
    (NOW() - INTERVAL '2 week')
  )
)
)
  INTO v_change_ids;
FOREACH v_change_id IN ARRAY v_change_ids
LOOP
  v_changeset_id = uuid_generate_v4();
  SELECT changes.pipeline_id
  FROM changes
  WHERE id = v_change_id
  INTO v_pipeline_id;
  INSERT INTO changesets(id, pipeline_id, status, delivered_at, delivered_by)
  VALUES (v_changeset_id, v_pipeline_id, 'closed','1970-01-01 00:00:00+00', 'pre_changeset_change');
  UPDATE changes
  SET changeset_id = v_changeset_id,
  delivered_by = 'pre_changeset_change',
  delivered_at = '1970-01-01 00:00:00+00'
  WHERE id = v_change_id;
END LOOP;
END;
$$;

SELECT * FROM create_default_changesets();

DROP FUNCTION create_default_changesets();

COMMIT;

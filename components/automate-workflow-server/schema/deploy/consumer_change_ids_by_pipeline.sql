-- Deploy delivery:consumer_change_ids_by_pipeline to pg

BEGIN;
CREATE OR REPLACE FUNCTION consumer_change_ids_by_pipeline(
  p_pipeline_id pipelines.id%TYPE)
RETURNS TABLE(
    consumer_change_ids UUID[])
LANGUAGE plpgsql
AS $$
DECLARE
    v_consumer_change_ids UUID[];
BEGIN

  -- Most recent change per a pipeline
  WITH latest_changesets AS (
    SELECT DISTINCT ON (pipeline_id) cs.*
    FROM changesets AS cs
    WHERE cs.status='closed'
    ORDER BY cs.pipeline_id, cs.delivered_at DESC
  )
  SELECT INTO v_consumer_change_ids ARRAY(SELECT latest_change_id
  FROM latest_changesets
  WHERE p_pipeline_id = ANY (latest_changesets.dependencies));

  RETURN QUERY VALUES (v_consumer_change_ids);
END;
$$;

COMMENT ON FUNCTION consumer_change_ids_by_pipeline(p_pipeline_id pipelines.id%TYPE) IS
$$
For a pipeline id, finds the most recent changes to enter union, which depend on it.
$$;
COMMIT;

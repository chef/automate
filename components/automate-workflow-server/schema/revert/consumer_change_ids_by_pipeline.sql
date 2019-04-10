-- Revert delivery:consumer_change_ids_by_pipeline from pg

BEGIN;

DROP FUNCTION IF EXISTS consumer_change_ids_by_pipeline(
  p_pipeline_id pipelines.id%TYPE);

COMMIT;

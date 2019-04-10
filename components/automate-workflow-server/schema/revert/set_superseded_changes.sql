-- Deploy delivery:set_superseded_changes to pg

BEGIN;

CREATE OR REPLACE FUNCTION set_superseded_changes(p_superseding_change_id changes.id%TYPE)
RETURNS SETOF changes
LANGUAGE plpgsql
ROWS 1
AS $$
DECLARE
  v_pipeline_id pipelines.id%TYPE;
  v_superseded_change changes%ROWTYPE;
  v_superseding_change_approved_at timestamp with time zone;
BEGIN

SELECT pipeline_id
  FROM changes
 WHERE id = p_superseding_change_id
  INTO v_pipeline_id;

  SELECT approved_at
    FROM changes
   WHERE id = p_superseding_change_id
    INTO v_superseding_change_approved_at;

UPDATE changes
   SET superseding_change_id = p_superseding_change_id
 WHERE pipeline_id = v_pipeline_id
   AND merge_sha IS NOT NULL
   AND delivered_by IS NULL
   AND id != p_superseding_change_id
   AND (approved_at < v_superseding_change_approved_at
     OR approved_at IS NULL);

UPDATE changes
   SET superseding_change_id = null
 WHERE id = p_superseding_change_id;

SELECT *
  FROM changes
 WHERE superseding_change_id = p_superseding_change_id
  INTO v_superseded_change;

RETURN NEXT v_superseded_change;
RETURN;

END;
$$;

COMMIT;

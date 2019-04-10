-- Deploy add_to_changeset

BEGIN;

CREATE OR REPLACE FUNCTION add_to_changeset(p_change_id changes.id%TYPE)
RETURNS SETOF changesets
LANGUAGE plpgsql
ROWS 1
AS $$
DECLARE
  v_pipeline_id pipelines.id%TYPE;
  v_changeset changesets%ROWTYPE;
BEGIN

SELECT pipeline_id
FROM changes
WHERE id = p_change_id
INTO v_pipeline_id;

IF NOT FOUND THEN
  RAISE EXCEPTION
    USING ERRCODE = 'CD018',
          MESSAGE = 'Unknown change',
          DETAIL  = 'Change "' || p_change_id || '" does not exist',
          HINT    = 'Cannot merge thin air';
END IF;

SELECT *
FROM changesets
WHERE pipeline_id = v_pipeline_id
  AND status = 'open'
INTO v_changeset;

-- if there's no open changeset for that pipeline,
-- let's create it
IF NOT FOUND THEN
  INSERT INTO changesets(id, pipeline_id, status)
  VALUES (uuid_generate_v4(), v_pipeline_id, 'open')
  RETURNING * INTO v_changeset;
END IF;

UPDATE changes
   SET changeset_id = v_changeset.id
WHERE id = p_change_id;

RETURN NEXT v_changeset;
RETURN;

END;
$$;

COMMIT;

-- Deploy phases_for_stage

BEGIN;

CREATE OR REPLACE FUNCTION phases_for_stage(p_stage_name stage_runs.stage%TYPE)
RETURNS SETOF TEXT
LANGUAGE SQL IMMUTABLE
AS $$
  SELECT phase
  FROM stage_ordering
  WHERE stage = p_stage_name
  ORDER BY sequence_number;
$$;

COMMENT ON FUNCTION all_stages() IS
$$Returns a list of all phases for a given stage, in pipeline-order.

It is marked as immutable because the contents of the stage_ordering
table shouldn't ever change.
$$;


COMMIT;

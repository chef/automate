-- Deploy all_stages

BEGIN;

CREATE OR REPLACE FUNCTION all_stages()
RETURNS SETOF TEXT
LANGUAGE SQL IMMUTABLE
AS $$
WITH work AS(
  SELECT sequence_number, stage, max(sequence_number) OVER (partition by stage)
  FROM stage_ordering)
SELECT stage
FROM work
WHERE sequence_number = max
ORDER by sequence_number;
$$;

COMMENT ON FUNCTION all_stages() IS
$$Returns a list of all stages, in pipeline-order.

It is marked as immutable because the contents of the stage_ordering
table shouldn't ever change.
$$;

COMMIT;

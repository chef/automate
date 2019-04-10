-- Verify stage_ordering

BEGIN;

SELECT sequence_number,
       stage,
       phase,
       parallel
FROM stage_ordering WHERE FALSE;

ROLLBACK;

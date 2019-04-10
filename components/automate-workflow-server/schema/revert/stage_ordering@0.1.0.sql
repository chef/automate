-- Revert stage_ordering

BEGIN;

DROP TABLE IF EXISTS stage_ordering;

COMMIT;

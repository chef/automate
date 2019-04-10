-- Revert enterprise_default_searches

BEGIN;

DROP TABLE IF EXISTS enterprise_default_searches;

COMMIT;

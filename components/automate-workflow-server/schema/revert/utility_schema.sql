-- Revert utility_schema

BEGIN;

DROP SCHEMA IF EXISTS utility;

COMMIT;

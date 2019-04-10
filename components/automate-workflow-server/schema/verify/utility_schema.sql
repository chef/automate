-- Verify utility_schema

BEGIN;

SELECT 1/COUNT(*)
  FROM information_schema.schemata
 WHERE schema_name = 'utility';

ROLLBACK;

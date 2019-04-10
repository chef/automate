-- Revert dependency_failures

BEGIN;

  DROP TABLE dependency_failures;

COMMIT;

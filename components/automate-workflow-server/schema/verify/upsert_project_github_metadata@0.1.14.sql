-- Verify upsert_project_github_metadata

BEGIN;

SELECT has_function_privilege(
  'upsert_project_github_metadata(bigint, text, text, text)',
  'execute');

ROLLBACK;

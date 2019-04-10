-- Verify delivery:dependencies_by_project on pg

BEGIN;

SELECT has_function_privilege(
  'dependencies_by_project(text, text, text)',
  'execute');

ROLLBACK;

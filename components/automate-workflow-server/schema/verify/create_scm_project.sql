-- Verify delivery:create_scm_project on pg

BEGIN;

SELECT has_function_privilege(
  'create_scm_project(text, text, text, text, text, text, text)',
  'execute');

ROLLBACK;

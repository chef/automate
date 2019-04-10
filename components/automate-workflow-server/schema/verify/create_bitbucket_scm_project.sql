-- Verify delivery:create_bitbucket_scm_project on pg

BEGIN;

SELECT has_function_privilege(
  'create_bitbucket_scm_project(text, text, text, text, text, text, text)',
  'execute');

ROLLBACK;

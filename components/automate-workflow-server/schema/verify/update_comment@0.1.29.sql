-- Verify insert_comment_comment

BEGIN;

SELECT has_function_privilege(
  'update_comment()',
  'execute');

ROLLBACK;

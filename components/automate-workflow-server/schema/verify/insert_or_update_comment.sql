-- Verify insert_or_update_comment

BEGIN;

SELECT has_function_privilege(
  'insert_or_update_comment(text, text, uuid, smallint, bigint, cd_comment_type, text, int4range, bigint, cd_comment_status, text)',
  'execute');

ROLLBACK;

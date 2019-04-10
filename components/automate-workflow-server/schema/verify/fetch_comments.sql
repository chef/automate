-- Verify fetch_comments

BEGIN;

SELECT has_function_privilege(
  'fetch_comments(uuid, smallint, cd_comment_type, text, bigint, boolean)',
  'execute');

ROLLBACK;

-- Verify delivery:add_status_comment_id_to_github_patchsets on pg

BEGIN;

SELECT status_comment_id
  FROM github_patchsets
 WHERE FALSE;

ROLLBACK;

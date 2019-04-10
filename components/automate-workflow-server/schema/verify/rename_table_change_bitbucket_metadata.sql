-- Verify delivery:rename_table_change_bitbucket_metadata on pg

BEGIN;

SELECT id, change_id, pr_id, pr_url
FROM scm_changes
WHERE FALSE;

ROLLBACK;

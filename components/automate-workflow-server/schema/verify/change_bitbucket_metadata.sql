-- Verify delivery:change_bitbucket_metadata on pg

BEGIN;

SELECT id, change_id, pr_id, pr_url
FROM change_bitbucket_metadata
WHERE FALSE;

ROLLBACK;

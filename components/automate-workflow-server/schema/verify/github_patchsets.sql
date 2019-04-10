-- Verify delivery:github_patchsets on pg

BEGIN;

SELECT id, patchset_id, payload
FROM github_patchsets
WHERE FALSE;


ROLLBACK;

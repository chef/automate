-- Verify patchsets

BEGIN;

SELECT id, change_id, sequence_number, submitted_at, submitter_id, verified_against_sha, is_verified, status
FROM patchsets
WHERE FALSE;

ROLLBACK;

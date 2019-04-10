-- Verify most_recent_patchsets_view

BEGIN;

SELECT id,
       change_id,
       sequence_number,
       submitted_at,
       sha,
       submitter_id,
       verified_against_sha,
       is_verified,
       status
FROM most_recent_patchsets
WHERE false;

ROLLBACK;

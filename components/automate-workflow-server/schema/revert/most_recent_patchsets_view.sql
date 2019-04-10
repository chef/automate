-- Revert most_recent_patchsets_view

BEGIN;

DROP VIEW IF EXISTS most_recent_patchsets;

COMMIT;

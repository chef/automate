-- Deploy most_recent_patchsets_view
-- requires: patchsets

BEGIN;

CREATE VIEW most_recent_patchsets AS
SELECT *
FROM patchsets
WHERE status != 'superseded';

COMMENT ON VIEW most_recent_patchsets IS
$$Shows all the most recent patchsets (i.e., those that have not been
superceded) for all changes in the system.$$;

COMMIT;

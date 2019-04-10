-- Verify cd_patchset_status

BEGIN;

-- If the type isn't there, this will fail horribly, as we want.
SELECT 'open'::cd_patchset_status;

ROLLBACK;

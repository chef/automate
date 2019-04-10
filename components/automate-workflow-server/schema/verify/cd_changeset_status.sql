-- Verify cd_changeset_status

BEGIN;

SELECT 'open'::cd_changeset_status;

ROLLBACK;

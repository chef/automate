-- Verify patchset_changed_files

BEGIN;

SELECT patchset_id,
       status,
       file
  FROM patchset_changed_files WHERE FALSE;

ROLLBACK;

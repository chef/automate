-- Verify patchset_diffstats

BEGIN;

SELECT id,
       files_changed,
       insertions,
       deletions
  FROM patchset_diffstats WHERE FALSE;

ROLLBACK;

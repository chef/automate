-- Revert delivery:fix_change_patchset_info from pg

BEGIN;

-- Since this script was meant to fix data, there is no reason to revert it
-- because that would just re-introduce the bug.

COMMIT;

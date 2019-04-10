-- Revert assert_patchset_in_change

BEGIN;

DROP FUNCTION IF EXISTS assert_patchset_in_change(
  p_patchset_id patchsets.id%TYPE,
  p_change_id changes.id%TYPE
);

COMMIT;

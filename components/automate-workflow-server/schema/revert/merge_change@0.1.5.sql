-- revert merge_change
BEGIN;

DROP FUNCTION IF EXISTS merge_change(
  p_change_id changes.id%TYPE,
  p_merge_sha changes.merge_sha%TYPE
);

COMMIT;

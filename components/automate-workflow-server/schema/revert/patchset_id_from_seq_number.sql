-- Revert patchset_id_from_seq_number

BEGIN;

DROP FUNCTION IF EXISTS patchset_id_from_seq_number(
  p_patchset_sequence_number patchsets.sequence_number%TYPE,
  p_change_id changes.id%TYPE
);

COMMIT;

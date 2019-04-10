-- Deploy patchset_id_from_seq_number
-- requires: patchsets

-- This function simply returns the patchset_id from the given change_id
-- and sequence number.
-- Throws an explicit exception if not found.

BEGIN;

CREATE OR REPLACE FUNCTION patchset_id_from_seq_number(
  p_patchset_sequence_number patchsets.sequence_number%TYPE,
  p_change_id changes.id%TYPE
)
RETURNS comments.patchset_id%TYPE
LANGUAGE plpgsql STABLE
AS $$
DECLARE
  v_patchset_id comments.patchset_id%TYPE;
BEGIN
  SELECT id
  FROM patchsets
  WHERE change_id = p_change_id
  AND sequence_number = p_patchset_sequence_number
  INTO v_patchset_id;

  IF NOT FOUND THEN
    RAISE EXCEPTION
    USING ERRCODE = 'CD017',
          MESSAGE = 'Unknown sequence number',
          DETAIL  = 'Change ' || p_change_id || ' does not have a patchset number ' || p_patchset_sequence_number,
          HINT    = 'T''is the hard cold truth buddy';
  END IF;

  RETURN v_patchset_id;
END;
$$;

COMMIT;

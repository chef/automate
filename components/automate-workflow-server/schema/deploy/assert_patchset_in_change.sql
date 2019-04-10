-- Deploy assert_patchset_in_change
-- requires: patchsets

-- This function simply checks that the given patchset is indeed part of the
-- given change, and throws an exception if it's not the case.

BEGIN;

CREATE OR REPLACE FUNCTION assert_patchset_in_change(
  p_patchset_id patchsets.id%TYPE,
  p_change_id changes.id%TYPE
)
RETURNS VOID
LANGUAGE plpgsql STABLE
AS $$
DECLARE
  detail TEXT;
BEGIN
  IF p_patchset_id IS NULL THEN
    detail = 'No patchsed ID given';
  ELSE
    PERFORM 1
    FROM patchsets
    WHERE id = p_patchset_id
    AND change_id = p_change_id;

    IF FOUND THEN
      RETURN;
    END IF;

    detail = 'Patchset ' || p_patchset_id || ' does not belong to change ' || p_change_id;
  END IF;

  RAISE EXCEPTION
  USING ERRCODE = 'CD015',
        MESSAGE = 'Unrelated change and patchset',
        DETAIL  = detail,
        HINT    = 'T''is the hard cold truth buddy';
END;
$$;

COMMIT;

-- Deploy get_patchsets_for_change

BEGIN;

CREATE OR REPLACE FUNCTION get_patchsets_for_change(p_change_id changes.id%TYPE)
RETURNS SETOF patchsets
LANGUAGE plpgsql STABLE
AS $$
DECLARE
  latest_patchset patchsets%ROWTYPE;
BEGIN

  SELECT * FROM get_latest_patchset(p_change_id)
  INTO latest_patchset;

  -- If latest_patchset is empty, the change wasn't found!
  IF latest_patchset.status IS NULL THEN
    RETURN;
  END IF;

  CASE latest_patchset.status
  WHEN 'merged' THEN
     -- Return only the latest (merged) patchset
     RETURN NEXT latest_patchset;
     RETURN;
  WHEN 'open' THEN
     -- return all patchsets
     RETURN QUERY
     -- The * is OK here, since we're by definition returning patchset
     -- records (i.e. `RETURNS SETOF patchsets`)
     SELECT p.*
     FROM patchsets AS p
     WHERE p.change_id = p_change_id
     ORDER BY p.sequence_number DESC;
  ELSE
    RAISE EXCEPTION
    USING ERRCODE = 'CD016',
          MESSAGE = 'Invalid state for change',
          DETAIL  = 'Change ' || p_change_id || ' appears to be in state ' || latest_patchset.status || ', which should never happen';
  END CASE;
END;
$$;

COMMENT ON FUNCTION get_patchsets_for_change(p_change_id changes.id%TYPE) IS
$$Returns the patchsets available for a given change, respecting the
status of that change; please call this function in favor of directly
SELECTing from the patchsets table.

If the change has been merged, then the Git branches supporting all
but the final patchset (the one that was finally merged) will have
been deleted; thus, we only return the single, final patchset record
for such a change. (Were we to do otherwise, we would ultimately try
to retrieve information from the project's Git repository on branches
that no longer exist, which would be bad.)

For changes that remain open, we return all patchset information,
ordered in reverse sequence-number order (that is, the most recent
patchset is first).

$$;

COMMIT;

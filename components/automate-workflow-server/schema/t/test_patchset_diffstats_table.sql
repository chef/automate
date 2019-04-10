CREATE OR REPLACE FUNCTION test_patchset_diffstats_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT has_table('patchset_diffstats');

-- Columns
  RETURN QUERY SELECT columns_are('patchset_diffstats', ARRAY['id',
                                                              'files_changed',
                                                              'insertions',
                                                              'deletions']);

  RETURN QUERY SELECT col_is_pk('patchset_diffstats', 'id');

END;
$$;

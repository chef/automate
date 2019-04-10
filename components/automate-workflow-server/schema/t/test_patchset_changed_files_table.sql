CREATE OR REPLACE FUNCTION test_patchset_changed_files_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT has_table('patchset_changed_files');

-- Columns
  RETURN QUERY SELECT columns_are('patchset_changed_files', ARRAY['patchset_id',
                                                                  'status',
                                                                  'file',
                                                                  'inserts',
                                                                  'deletes']);

  RETURN QUERY SELECT col_is_unique('patchset_changed_files', ARRAY['patchset_id', 'file']);

END;
$$;

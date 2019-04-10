CREATE OR REPLACE FUNCTION test_github_patchsets_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT has_table('github_patchsets');

  -- Columns
  RETURN QUERY SELECT columns_are('github_patchsets', ARRAY['id',
                                                            'patchset_id',
                                                            'payload',
                                                            'status_comment_id'
                                                           ]);

  RETURN QUERY SELECT col_is_pk('github_patchsets', 'id');

END;
$$;

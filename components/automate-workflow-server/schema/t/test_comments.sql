CREATE OR REPLACE FUNCTION test_comments_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT has_table('comments');

  -- Columns
  RETURN QUERY SELECT columns_are('comments', ARRAY['id',
                                                    'patchset_id',
                                                    'submitter_id',
                                                    'content',
                                                    'type',
                                                    'status',
                                                    'last_modif_or_publication_timestamp',
                                                    'line_range',
                                                    'file_path',
                                                    'parent_id'
                                                    ]);

  RETURN QUERY SELECT col_is_pk('comments', 'id');

  RETURN QUERY SELECT indexes_are('comments',
    ARRAY['comments_pkey', 'patchset_id_file_path_idx', 'parent_id_idx']);

END;
$$;

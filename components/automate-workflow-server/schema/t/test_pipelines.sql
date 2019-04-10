CREATE OR REPLACE FUNCTION test_pipelines_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT has_table('pipelines');

  -- Columns
  RETURN QUERY SELECT columns_are('pipelines', ARRAY['id',
                                                     'project_id',
                                                     'name'
                                                    ]);

  RETURN QUERY SELECT col_is_pk('pipelines', 'id');
  RETURN QUERY SELECT col_is_unique('pipelines', ARRAY['project_id', 'name']);

END;
$$;

CREATE OR REPLACE FUNCTION test_project_bitbucket_metadata_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT has_table('project_bitbucket_metadata');

  -- Columns
  RETURN QUERY SELECT columns_are('project_bitbucket_metadata',
                                  ARRAY['project_id',
                                        'bitbucket_project',
                                        'repo_name'
                                        ]);

  RETURN QUERY SELECT col_is_pk('project_bitbucket_metadata', 'project_id');
  RETURN QUERY SELECT col_is_unique('project_bitbucket_metadata', ARRAY['bitbucket_project', 'repo_name']);

END;
$$;

CREATE OR REPLACE FUNCTION test_project_github_metadata_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT has_table('project_github_metadata');

  -- Columns
  RETURN QUERY SELECT columns_are('project_github_metadata',
                                  ARRAY['project_id',
                                        'repo_owner',
                                        'repo_name',
                                        'token'
                                        ]);

  RETURN QUERY SELECT col_is_pk('project_github_metadata', 'project_id');
  RETURN QUERY SELECT col_is_unique('project_github_metadata', ARRAY['repo_owner', 'repo_name']);

END;
$$;

CREATE OR REPLACE FUNCTION test_project_commits_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT has_table('project_commits');

-- Columns
  RETURN QUERY SELECT columns_are('project_commits', ARRAY['id',
                                                           'project_id',
                                                           'sha',
                                                           'subject',
                                                           'body']);

  RETURN QUERY SELECT col_is_pk('project_commits', 'id');
  RETURN QUERY SELECT col_is_unique('project_commits', ARRAY['project_id', 'sha']);

END;
$$;

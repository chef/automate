CREATE OR REPLACE FUNCTION test_patchset_project_commits_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT has_table('patchset_project_commits');

-- Columns
  RETURN QUERY SELECT columns_are('patchset_project_commits', ARRAY['patchset_id',
                                                                    'project_commit_id']);

  RETURN QUERY SELECT col_is_unique('patchset_project_commits', ARRAY['patchset_id', 'project_commit_id']);

END;
$$;

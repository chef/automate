CREATE OR REPLACE FUNCTION test_teams_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT has_table('teams');

-- Columns
  RETURN QUERY SELECT columns_are('teams', ARRAY['id',
                                                 'enterprise_id',
                                                 'name',
                                                 'description',
                                                 'creator_id',
                                                 'created_at',
                                                 'updater_id',
                                                 'updated_at']);

  RETURN QUERY SELECT col_is_pk('teams', 'id');
  RETURN QUERY SELECT col_is_unique('teams', ARRAY['enterprise_id', 'name']);

END;
$$;

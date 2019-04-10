CREATE OR REPLACE FUNCTION test_team_creators_team_created_table()

RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT columns_are('teams', ARRAY['id',
                                                 'enterprise_id',
                                                 'name',
                                                 'description',
                                                 'creator_id',
                                                 'updater_id',
                                                 'created_at',
                                                 'updated_at']);

END;
$$;

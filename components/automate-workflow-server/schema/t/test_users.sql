CREATE OR REPLACE FUNCTION test_users_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT has_table('users');

-- Columns
  RETURN QUERY SELECT columns_are('users', ARRAY['id',
                                                 'enterprise_id',
                                                 'name',
                                                 'ssh_pub_key',
                                                 'first_name',
                                                 'last_name',
                                                 'email',
                                                 'user_type', 
                                                 'telemetry_enabled'
                                                 ]);

  RETURN QUERY SELECT col_is_pk('users', 'id');
  RETURN QUERY SELECT col_is_unique('users', ARRAY['enterprise_id', 'name']);

END;
$$;

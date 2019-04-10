CREATE OR REPLACE FUNCTION test_external_basic_auth_applications_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT has_table('external_basic_auth_applications');

-- Columns
  RETURN QUERY SELECT columns_are('external_basic_auth_applications', ARRAY['name',
                                                                            'root_api_url',
                                                                            'user_id',
                                                                            'password',
                                                                            'ent_id'
                                                                           ]);

  RETURN QUERY SELECT col_is_pk('external_basic_auth_applications', ARRAY['name', 'ent_id']);
  RETURN QUERY SELECT col_is_fk('external_basic_auth_applications', 'ent_id');
  RETURN QUERY SELECT col_is_unique('external_basic_auth_applications', 'name');

END;
$$;

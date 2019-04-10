CREATE OR REPLACE FUNCTION test_external_oauth_applications_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT has_table('external_oauth_applications');

-- Columns
  RETURN QUERY SELECT columns_are('external_oauth_applications', ARRAY['id',
                                                                       'name',
                                                                       'module',
                                                                       'root_url',
                                                                       'root_api_url',
                                                                       'client_id',
                                                                       'client_secret'
                                                                       ]);

  RETURN QUERY SELECT col_is_pk('external_oauth_applications', 'id');
  RETURN QUERY SELECT col_is_unique('external_oauth_applications', 'name');

END;
$$;

CREATE OR REPLACE FUNCTION test_oauth_user_aliases()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT has_table('oauth_user_aliases');

-- Columns
  RETURN QUERY SELECT columns_are('oauth_user_aliases', ARRAY['id',
                                                              'user_id',
                                                              'oauth_app_id',
                                                              'alias'
                                                             ]);

  RETURN QUERY SELECT col_is_pk('oauth_user_aliases', 'id');
  RETURN QUERY SELECT col_is_unique('oauth_user_aliases', ARRAY['oauth_app_id', 'user_id']);
  RETURN QUERY SELECT col_is_unique('oauth_user_aliases', ARRAY['oauth_app_id', 'alias']);
  RETURN QUERY SELECT chef_pgtap.fk_delete_action_is('oauth_user_aliases_user_id_fkey', 'CASCADE');
  RETURN QUERY SELECT chef_pgtap.fk_delete_action_is('oauth_user_aliases_oauth_app_id_fkey', 'CASCADE');

END;
$$;

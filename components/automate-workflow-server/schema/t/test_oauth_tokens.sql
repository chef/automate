CREATE OR REPLACE FUNCTION test_oauth_tokens()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT has_table('oauth_tokens');

  -- Columns
  RETURN QUERY SELECT columns_are('oauth_tokens',
                                  ARRAY['id',
                                        'oauth_app_id',
                                        'scope',
                                        'scope_id',
                                        'state',
                                        'token'
                                        ]);

  RETURN QUERY SELECT col_is_pk('oauth_tokens', 'id');
  RETURN QUERY SELECT col_is_fk('oauth_tokens', 'oauth_app_id');
  RETURN QUERY SELECT col_is_unique('oauth_tokens', 'state');
  RETURN QUERY SELECT col_is_unique('oauth_tokens', ARRAY['oauth_app_id', 'scope', 'scope_id']);
  RETURN QUERY SELECT chef_pgtap.fk_delete_action_is('oauth_tokens_oauth_app_id_fkey', 'CASCADE');

END;
$$;

CREATE OR REPLACE FUNCTION test_scm_changes()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT has_table('scm_changes');

  -- Columns
  RETURN QUERY SELECT columns_are('scm_changes',
                                  ARRAY['id',
                                        'change_id',
                                        'pr_id',
                                        'pr_url'
                                        ]);

  RETURN QUERY SELECT col_is_pk('scm_changes', 'id');
  RETURN QUERY SELECT col_is_unique('scm_changes', ARRAY['change_id']);

END;
$$;

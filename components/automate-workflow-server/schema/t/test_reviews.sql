CREATE OR REPLACE FUNCTION test_reviews_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT has_table('reviews');

  -- Columns
  RETURN QUERY SELECT columns_are('reviews', ARRAY['id',
                                                   'patchset_id',
                                                   'user_id',
                                                   'numeric_value'
                                                   ]);

  RETURN QUERY SELECT col_is_pk('reviews', 'id');
  RETURN QUERY SELECT col_is_unique('reviews', ARRAY['patchset_id', 'user_id']);

END;
$$;

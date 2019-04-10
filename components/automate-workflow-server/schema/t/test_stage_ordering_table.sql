CREATE OR REPLACE FUNCTION test_stage_ordering_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT has_table('stage_ordering');

  -- Columns
  RETURN QUERY SELECT columns_are('stage_ordering', ARRAY['sequence_number',
                                                       'stage',
                                                       'phase',
                                                       'parallel']);

  RETURN QUERY SELECT col_is_pk('stage_ordering', ARRAY['stage', 'phase']);
  RETURN QUERY SELECT col_is_unique('stage_ordering', 'sequence_number');

END;
$$;

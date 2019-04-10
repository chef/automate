CREATE OR REPLACE FUNCTION test_phase_run_logs_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT has_table('phase_run_logs');

  -- Columns
  RETURN QUERY SELECT columns_are('phase_run_logs', ARRAY['id',
                                                          'run_id',
                                                          'data'
                                                        ]);

  RETURN QUERY SELECT col_is_pk('phase_run_logs', 'id');

END;
$$;

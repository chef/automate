CREATE OR REPLACE FUNCTION test_phase_runs_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT has_table('phase_runs');

  -- Columns
  RETURN QUERY SELECT columns_are('phase_runs', ARRAY['id',
                                                     'stage_run_id',
                                                     'phase',
                                                     'status',
                                                     'finished',
                                                     'run_success',
                                                     'run_log',
                                                     'run_status',
                                                     'build_node',
                                                     'search_query',
                                                     'search_description',
                                                     'description',
                                                     'created_at',
                                                     'started_at',
                                                     'finished_at'
                                                    ]);

  RETURN QUERY SELECT col_is_pk('phase_runs', 'id');

END;
$$;

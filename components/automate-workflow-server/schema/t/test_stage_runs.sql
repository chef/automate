CREATE OR REPLACE FUNCTION test_stage_runs_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT has_table('stage_runs');

  -- Columns
  RETURN QUERY SELECT columns_are('stage_runs', ARRAY['id',
                                                     'change_id',
                                                     'stage',
                                                     'status',
                                                     'finished',
                                                     'created_at',
                                                     'started_at',
                                                     'finished_at'
                                                    ]);

  RETURN QUERY SELECT col_is_pk('stage_runs', 'id');

  RETURN QUERY SELECT chef_pgtap.fk_delete_action_is('stage_runs_change_id_fkey', 'CASCADE');
  RETURN QUERY SELECT chef_pgtap.fk_update_action_is('stage_runs_change_id_fkey', 'CASCADE');

END;
$$;

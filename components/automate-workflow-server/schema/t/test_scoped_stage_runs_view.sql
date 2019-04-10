CREATE OR REPLACE FUNCTION test_scoped_stage_runs_view()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
DECLARE

  stage_runs_columns TEXT[]:= ARRAY['id',
                                    'change_id',
                                    'stage',
                                    'status',
                                    'finished'];
  extra_columns TEXT[]:= ARRAY['enterprise_id',
                               'organization_id',
                               'project_id',
                               'pipeline_id'];
BEGIN

  RETURN QUERY SELECT has_view('scoped_stage_runs');

  -- For now, we'll just assert that the column names are the same,
  -- even though the types should also be the same.
  RETURN QUERY SELECT columns_are('scoped_stage_runs', extra_columns || stage_runs_columns );

END;
$$;

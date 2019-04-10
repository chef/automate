CREATE OR REPLACE FUNCTION test_changesets_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
DECLARE
  feature_branch_pipeline_id_not_merged_idx_def TEXT;
BEGIN
  RETURN QUERY SELECT has_table('changesets');

  -- Columns
  RETURN QUERY SELECT columns_are('changesets', ARRAY['id',
                                                      'pipeline_id',
                                                      'status',
                                                      'delivered_at',
                                                      'delivered_by',
                                                      'dependencies',
                                                      'latest_change_id'
                                                    ]);

  RETURN QUERY SELECT col_is_pk('changesets', 'id');
  RETURN QUERY SELECT indexes_are('changesets',
    ARRAY['changesets_pkey', 'single_open_changeset_idx']);

  RETURN QUERY SELECT results_eq(
    'SELECT indexdef '
    'FROM pg_indexes '
    'WHERE tablename = ''changesets'' '
    'AND indexname = ''single_open_changeset_idx''',
    ARRAY['CREATE UNIQUE INDEX single_open_changeset_idx ON public.changesets USING btree (pipeline_id) WHERE (status = ''open''::cd_changeset_status)']);
END;
$$;

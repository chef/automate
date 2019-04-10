CREATE OR REPLACE FUNCTION test_changes_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN
  RETURN QUERY SELECT has_table('changes');

  -- Columns
  RETURN QUERY SELECT columns_are('changes', ARRAY['id',
                                                     'pipeline_id',
                                                     'feature_branch',
                                                     'merge_sha',
                                                     'title',
                                                     'description',
                                                     'approved_by',
                                                     'approved_at',
                                                     'changeset_id',
                                                     'latest_patchset',
                                                     'latest_patchset_status',
                                                     'submitted_by',
                                                     'submitted_at',
                                                     'delivered_at',
                                                     'delivered_by',
                                                     'pipeline_name_at_creation',
                                                     'superseding_change_id'
                                                    ]);

  RETURN QUERY SELECT col_is_pk('changes', 'id');
  RETURN QUERY SELECT indexes_are('changes',
    ARRAY['changes_pkey', 'feature_branch_pipeline_id_not_merged_idx']);

  -- The following's rather ugly, but couldn't find a better way to test
  -- the existence of a partial index with pgTAP (??)
  -- If nothing else, this should be made part of chef_pgtap, as
  -- `has_partial_index(:table_name, :index_name, :indexed_columns, :conditions, :is_unique)'
  -- maybe we could also use pg_index.indpred (http://www.postgresql.org/docs/9.3/static/catalog-pg-index.html) ?

  RETURN QUERY SELECT results_eq(
    'SELECT indexdef '
    'FROM pg_indexes '
    'WHERE tablename = ''changes'' '
    'AND indexname = ''feature_branch_pipeline_id_not_merged_idx''',
    ARRAY['CREATE UNIQUE INDEX feature_branch_pipeline_id_not_merged_idx ON public.changes USING btree (pipeline_id, feature_branch) WHERE (merge_sha IS NULL)']);
END;
$$;

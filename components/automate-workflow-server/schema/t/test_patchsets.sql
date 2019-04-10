CREATE OR REPLACE FUNCTION test_patchsets_table()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT has_table('patchsets');

  -- Columns
  RETURN QUERY SELECT columns_are('patchsets', ARRAY['id',
                                                     'change_id',
                                                     'sequence_number',
                                                     'sha',
                                                     'submitted_at',
                                                     'submitter_id',
                                                     'verified_against_sha',
                                                     'is_verified',
                                                     'status'
                                                    ]);

  RETURN QUERY SELECT chef_pgtap.col_is_flag('patchsets', 'is_verified', FALSE);

  RETURN QUERY SELECT col_is_pk('patchsets', 'id');
  RETURN QUERY SELECT col_is_unique('patchsets', ARRAY['change_id', 'sequence_number']);

END;
$$;

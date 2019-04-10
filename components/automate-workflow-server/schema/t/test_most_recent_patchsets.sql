CREATE OR REPLACE FUNCTION test_most_recent_patchsets_view()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT has_view('most_recent_patchsets');

  -- Aww yeah, information schema :)
  PREPARE columns AS
  SELECT ordinal_position,
         column_name,
         data_type
  FROM information_schema.columns
  WHERE table_name = $1
  ORDER BY ordinal_position;

  RETURN QUERY SELECT results_eq(
    format('EXECUTE columns(%L)', 'most_recent_patchsets'),
    format('EXECUTE columns(%L)', 'patchsets'),
    'The "most_recent_patchsets" view should have the same structure as the "patchsets" table'
  );

END;
$$;

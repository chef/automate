CREATE OR REPLACE FUNCTION test_all_stages_function()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT results_eq(
    'SELECT * FROM all_stages()',
    ARRAY['verify', 'build', 'acceptance', 'union', 'rehearsal', 'delivered'],
    'The database knows about all the stages');

END;
$$;

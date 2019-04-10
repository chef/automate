CREATE OR REPLACE FUNCTION test_phases_for_stage_function()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT results_eq(
    format('SELECT * FROM phases_for_stage(%L)', 'verify'),
    ARRAY['unit', 'lint', 'syntax'],
    'All verify phases are present');

  RETURN QUERY SELECT results_eq(
    format('SELECT * FROM phases_for_stage(%L)', 'build'),
    ARRAY['unit', 'lint', 'syntax', 'quality', 'security', 'publish'],
    'All build phases are present');

  RETURN QUERY SELECT results_eq(
    format('SELECT * FROM phases_for_stage(%L)', 'acceptance'),
    ARRAY['provision', 'deploy', 'smoke', 'functional'],
    'All acceptance phases are present');

  RETURN QUERY SELECT results_eq(
    format('SELECT * FROM phases_for_stage(%L)', 'union'),
    ARRAY['provision', 'deploy', 'smoke', 'functional'],
    'All union phases are present');

  RETURN QUERY SELECT results_eq(
    format('SELECT * FROM phases_for_stage(%L)', 'rehearsal'),
    ARRAY['provision', 'deploy', 'smoke', 'functional'],
    'All rehearsal phases are present');

  RETURN QUERY SELECT results_eq(
    format('SELECT * FROM phases_for_stage(%L)', 'delivered'),
    ARRAY['provision', 'deploy', 'smoke', 'functional'],
    'All delivered phases are present');

END;
$$;

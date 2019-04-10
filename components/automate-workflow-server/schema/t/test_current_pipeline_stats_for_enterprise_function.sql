CREATE OR REPLACE FUNCTION test_current_pipelines_stats_for_enterprise_function()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

--SELECT FORMAT('(%s::bigint, %L, %L, %L, %s::bigint, %L, %s::bigint, %L, %L, %L),', id, org_name, proj_name, name, build_count, build_status, acceptance_count, acceptance_status, last_deployed, last_delivered) from current_pipelines_stats_for_enterprise('Chef');
--SELECT FORMAT('(%s::bigint, %L, %L, %L, %s::bigint, %L, %s::bigint, %L, %L, %L),', id, org_name, proj_name, name, build_count, build_status, acceptance_count, acceptance_status, last_deployed, last_delivered) from current_pipelines_stats_for_enterprise('BigCo');

  PREPARE chef_stats AS
  VALUES (2::bigint, 'Chef_Delivery', 'delivery', 'master', 0::bigint, 'passed', 0::bigint, 'passed', CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp), CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp)),
         (3::bigint, 'Chef_Delivery', 'delivery_web', 'master', 0::bigint, 'passed', 0::bigint, 'passed', CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp), CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp)),
         (4::bigint, 'Chef_Cookbooks', 'chef-server-12', 'master', 0::bigint, 'passed', 0::bigint, 'passed', CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp), CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp)),
         (5::bigint, 'Chef_Delivery', 'omnibus-delivery', 'master', 0::bigint, 'passed', 0::bigint, 'passed', CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp), CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp)),
         (8::bigint, 'Chef_Delivery_Cookbooks', 'delivery_chef', 'master', 0::bigint, 'passed', 0::bigint, 'passed', CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp), CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp)),
         (9::bigint, 'Chef_Delivery_Cookbooks', 'delivery_build_cbs', 'master', 0::bigint, 'passed', 1::bigint, 'running', CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp), CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp)),
         (11::bigint, 'sandbox', 'radar', 'master', 5::bigint, 'failed', 3::bigint, 'passed', CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp), CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp)),
         (12::bigint, 'Chef_Delivery_Cookbooks', 'delivery_github', 'master', 0::bigint, 'passed', 1::bigint, 'passed', CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp), CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp)),
         (14::bigint, 'Chef_Partners', 'azure-chef-extension', 'pipeline-test', 0::bigint, 'passed', 0::bigint, 'passed', CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp), CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp)),
         (15::bigint, 'adam_universe', 'delivery-cli', 'master', 0::bigint, 'passed', 0::bigint, 'passed', CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp), CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp)),
         (16::bigint, 'sandbox', 'test', 'master', 0::bigint, 'passed', 0::bigint, 'passed', CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp), CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp)),
         (17::bigint, 'Chef_Delivery', 'delivery-cli', 'master', 2::bigint, 'failed', 0::bigint, 'passed', CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp), CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp)),
         (18::bigint, 'Alex_Universe', 'greentea', 'master', 0::bigint, 'passed', 0::bigint, 'passed', CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp), CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp)),
         (19::bigint, 'Chef_Delivery', 'banana', 'master', 0::bigint, 'passed', 0::bigint, 'passed', CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp), CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp)),
         (20::bigint, 'almond', 'pants', 'master', 0::bigint, 'passed', 0::bigint, 'passed', CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp), CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp)),
         (21::bigint, 'almond', 'chess', 'master', 0::bigint, 'passed', 0::bigint, 'passed', CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp), CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp)),
         (22::bigint, 'sandbox', 'barrista', 'master', 0::bigint, 'passed', 0::bigint, 'passed', CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp), CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp)),
         (23::bigint, 'sandbox', 'game_of_thrones', 'master', 0::bigint, 'passed', 0::bigint, 'passed', CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp), CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp));

  -- The top finished comes from the sanatized dump and needs to be this way til we get a new dump without finished in it.
  PREPARE bigco_stats AS
  VALUES (24::bigint, 'BigCo Engineering', 'skunkworks', 'master', 0::bigint, 'finished', 0::bigint, 'passed', CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp), CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp)),
         (25::bigint, 'BigCo Engineering', 'skunkworks', 'v1', 0::bigint, 'passed', 0::bigint, 'passed', CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp), CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp)),
         (26::bigint, 'BigCo Engineering', 'skunkworks', 'legacy', 0::bigint, 'passed', 0::bigint, 'passed', CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp), CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp)),
         (28::bigint, 'BigCo Engineering', 'honeybadger', 'master', 0::bigint, 'passed', 0::bigint, 'passed', CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp), CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp));

  PREPARE smallco_stats AS
  VALUES (27::bigint, 'SmallCo Engineering', 'disrupt-o-tron', 'master', 0::bigint, 'passed', 0::bigint, 'passed', CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp), CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp));

  PREPARE stats AS
    SELECT * FROM current_pipelines_stats_for_enterprise($1);

  RETURN QUERY SELECT results_eq(
    format('EXECUTE stats(%L)', 'Chef'),
    'chef_stats',
    'Should retrieve the correct pipeline stats for Chef');

  RETURN QUERY SELECT results_eq(
    format('EXECUTE stats(%L)', 'BigCo'),
    'bigco_stats',
    'Should retrieve the correct pipeline stats for BigCo');

  RETURN QUERY SELECT results_eq(
    format('EXECUTE stats(%L)', 'SmallCo'),
    'smallco_stats',
    'Should retrieve the correct pipeline stats for SmallCo');

  RETURN QUERY SELECT is_empty(
    format('EXECUTE stats(%L)', 'ExternalCo'),
    'ExternalCo should have an empty pipeline stats');

  RETURN QUERY SELECT throws_ok(
    format('EXECUTE stats(%L)', 'ThisEnterpriseDoesNotExist'),
    'CD003',
    'Enterprise not found',
    'Throws an exception if the enterprise does not exist');

  -- Now that we know the function basically works, lets follow a
  -- change through the system by simulating several additional stage
  -- runs and verify that the stats changes as appropriate.

  -- There is a small bug that we have to have run one change through to union
  -- before the stats report right. TODO: Fixme
  INSERT INTO stage_runs(change_id, stage, status, finished) VALUES ('396bd34b-c42a-465e-85bd-a2c49b9908a6', 'verify', 'passed', true);
  INSERT INTO stage_runs(change_id, stage, status, finished) VALUES ('396bd34b-c42a-465e-85bd-a2c49b9908a6', 'build', 'passed', true);
  INSERT INTO stage_runs(change_id, stage, status, finished) VALUES ('396bd34b-c42a-465e-85bd-a2c49b9908a6', 'acceptance', 'passed', true);
  INSERT INTO stage_runs(change_id, stage, status, finished) VALUES ('396bd34b-c42a-465e-85bd-a2c49b9908a6', 'union', 'passed', true);

  -- Insert a build run that is running and see the stats
  INSERT INTO stage_runs(change_id, stage, status, finished) VALUES ('cb761c42-35e2-4e58-936e-27b381c01aab', 'verify', 'passed', true);
  INSERT INTO stage_runs(change_id, stage, status, finished) VALUES ('cb761c42-35e2-4e58-936e-27b381c01aab', 'build', 'running', false);

  PREPARE bigco_stats2 AS
  VALUES (24::bigint, 'BigCo Engineering', 'skunkworks', 'master', 1::bigint, 'running', 0::bigint, 'passed', CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp), CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp)),
         (25::bigint, 'BigCo Engineering', 'skunkworks', 'v1', 0::bigint, 'passed', 0::bigint, 'passed', CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp), CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp)),
         (26::bigint, 'BigCo Engineering', 'skunkworks', 'legacy', 0::bigint, 'passed', 0::bigint, 'passed', CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp), CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp)),
         (28::bigint, 'BigCo Engineering', 'honeybadger', 'master', 0::bigint, 'passed', 0::bigint, 'passed', CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp), CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp));

  RETURN QUERY SELECT results_eq(
    format('EXECUTE stats(%L)', 'BigCo'),
    'bigco_stats2',
    'Should retrieve the correct pipeline stats for BigCo');

  -- Update that last run to finished and failed and see that stats are right.
  UPDATE stage_runs SET status='failed', finished=true WHERE id IN
    (SELECT id FROM stage_runs WHERE change_id='cb761c42-35e2-4e58-936e-27b381c01aab' AND stage='build' ORDER BY id DESC LIMIT 1);

  PREPARE bigco_stats3 AS
  VALUES (24::bigint, 'BigCo Engineering', 'skunkworks', 'master', 1::bigint, 'failed', 0::bigint, 'passed', CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp), CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp)),
         (25::bigint, 'BigCo Engineering', 'skunkworks', 'v1', 0::bigint, 'passed', 0::bigint, 'passed', CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp), CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp)),
         (26::bigint, 'BigCo Engineering', 'skunkworks', 'legacy', 0::bigint, 'passed', 0::bigint, 'passed', CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp), CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp)),
         (28::bigint, 'BigCo Engineering', 'honeybadger', 'master', 0::bigint, 'passed', 0::bigint, 'passed', CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp), CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp));

  RETURN QUERY SELECT results_eq(
    format('EXECUTE stats(%L)', 'BigCo'),
    'bigco_stats3',
    'Should retrieve the correct pipeline stats for BigCo');


  -- Run another change to fix failure and check that stats are right.
  INSERT INTO stage_runs(change_id, stage, status, finished) VALUES ('7e1d8f8c-920f-4f6c-a87c-4ee3fea87654', 'verify', 'passed', true);
  INSERT INTO stage_runs(change_id, stage, status, finished) VALUES ('7e1d8f8c-920f-4f6c-a87c-4ee3fea87654', 'build', 'passed', true);

  PREPARE bigco_stats4 AS
  VALUES (24::bigint, 'BigCo Engineering', 'skunkworks', 'master', 2::bigint, 'passed', 0::bigint, 'passed', CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp), CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp)),
         (25::bigint, 'BigCo Engineering', 'skunkworks', 'v1', 0::bigint, 'passed', 0::bigint, 'passed', CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp), CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp)),
         (26::bigint, 'BigCo Engineering', 'skunkworks', 'legacy', 0::bigint, 'passed', 0::bigint, 'passed', CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp), CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp)),
         (28::bigint, 'BigCo Engineering', 'honeybadger', 'master', 0::bigint, 'passed', 0::bigint, 'passed', CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp), CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp));

  RETURN QUERY SELECT results_eq(
    format('EXECUTE stats(%L)', 'BigCo'),
    'bigco_stats4',
    'Should retrieve the correct pipeline stats for BigCo');

  -- Set change to running in acceptance since it passed build.
  INSERT INTO stage_runs(change_id, stage, status, finished) VALUES ('7e1d8f8c-920f-4f6c-a87c-4ee3fea87654', 'acceptance', 'running', false);

  PREPARE bigco_stats5 AS
  VALUES (24::bigint, 'BigCo Engineering', 'skunkworks', 'master', 0::bigint, 'passed', 1::bigint, 'running', CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp), CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp)),
         (25::bigint, 'BigCo Engineering', 'skunkworks', 'v1', 0::bigint, 'passed', 0::bigint, 'passed', CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp), CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp)),
         (26::bigint, 'BigCo Engineering', 'skunkworks', 'legacy', 0::bigint, 'passed', 0::bigint, 'passed', CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp), CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp)),
         (28::bigint, 'BigCo Engineering', 'honeybadger', 'master', 0::bigint, 'passed', 0::bigint, 'passed', CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp), CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp));

  RETURN QUERY SELECT results_eq(
    format('EXECUTE stats(%L)', 'BigCo'),
    'bigco_stats5',
    'Should retrieve the correct pipeline stats for BigCo');

  -- Update that last run to finished and failed and see that stats are right.
  UPDATE stage_runs SET status='failed', finished=true WHERE id IN
      (SELECT id FROM stage_runs WHERE change_id='7e1d8f8c-920f-4f6c-a87c-4ee3fea87654' AND stage='acceptance' ORDER BY id DESC LIMIT 1);

  PREPARE bigco_stats6 AS
  VALUES (24::bigint, 'BigCo Engineering', 'skunkworks', 'master', 0::bigint, 'passed', 1::bigint, 'failed', CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp), CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp)),
         (25::bigint, 'BigCo Engineering', 'skunkworks', 'v1', 0::bigint, 'passed', 0::bigint, 'passed', CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp), CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp)),
         (26::bigint, 'BigCo Engineering', 'skunkworks', 'legacy', 0::bigint, 'passed', 0::bigint, 'passed', CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp), CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp)),
         (28::bigint, 'BigCo Engineering', 'honeybadger', 'master', 0::bigint, 'passed', 0::bigint, 'passed', CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp), CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp));

  RETURN QUERY SELECT results_eq(
    format('EXECUTE stats(%L)', 'BigCo'),
    'bigco_stats6',
    'Should retrieve the correct pipeline stats for BigCo');

  -- Run another change to fix failure and check that stats are right stopping at build to see stats.
  INSERT INTO stage_runs(change_id, stage, status, finished) VALUES ('d83f9edd-66b1-44ac-b617-ffefa07660af', 'verify', 'passed', true);
  INSERT INTO stage_runs(change_id, stage, status, finished) VALUES ('d83f9edd-66b1-44ac-b617-ffefa07660af', 'build', 'passed', true);

  PREPARE bigco_stats7 AS
  VALUES (24::bigint, 'BigCo Engineering', 'skunkworks', 'master', 1::bigint, 'passed', 1::bigint, 'failed', CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp), CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp)),
         (25::bigint, 'BigCo Engineering', 'skunkworks', 'v1', 0::bigint, 'passed', 0::bigint, 'passed', CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp), CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp)),
         (26::bigint, 'BigCo Engineering', 'skunkworks', 'legacy', 0::bigint, 'passed', 0::bigint, 'passed', CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp), CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp)),
         (28::bigint, 'BigCo Engineering', 'honeybadger', 'master', 0::bigint, 'passed', 0::bigint, 'passed', CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp), CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp));

  RETURN QUERY SELECT results_eq(
    format('EXECUTE stats(%L)', 'BigCo'),
    'bigco_stats7',
    'Should retrieve the correct pipeline stats for BigCo');

  INSERT INTO stage_runs(change_id, stage, status, finished) VALUES ('d83f9edd-66b1-44ac-b617-ffefa07660af', 'acceptance', 'passed', true);

  PREPARE bigco_stats8 AS
  VALUES (24::bigint, 'BigCo Engineering', 'skunkworks', 'master', 0::bigint, 'passed', 2::bigint, 'passed', CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp), CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp)),
         (25::bigint, 'BigCo Engineering', 'skunkworks', 'v1', 0::bigint, 'passed', 0::bigint, 'passed', CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp), CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp)),
         (26::bigint, 'BigCo Engineering', 'skunkworks', 'legacy', 0::bigint, 'passed', 0::bigint, 'passed', CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp), CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp)),
         (28::bigint, 'BigCo Engineering', 'honeybadger', 'master', 0::bigint, 'passed', 0::bigint, 'passed', CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp), CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp));

  RETURN QUERY SELECT results_eq(
    format('EXECUTE stats(%L)', 'BigCo'),
    'bigco_stats8',
    'Should retrieve the correct pipeline stats for BigCo');

  -- Finally deliver it to union and check stats
  INSERT INTO stage_runs(change_id, stage, status, finished) VALUES ('d83f9edd-66b1-44ac-b617-ffefa07660af', 'union', 'passed', true);

  PREPARE bigco_stats9 AS
  VALUES (24::bigint, 'BigCo Engineering', 'skunkworks', 'master', 0::bigint, 'passed', 0::bigint, 'passed', CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp), CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp)),
         (25::bigint, 'BigCo Engineering', 'skunkworks', 'v1', 0::bigint, 'passed', 0::bigint, 'passed', CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp), CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp)),
         (26::bigint, 'BigCo Engineering', 'skunkworks', 'legacy', 0::bigint, 'passed', 0::bigint, 'passed', CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp), CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp)),
         (28::bigint, 'BigCo Engineering', 'honeybadger', 'master', 0::bigint, 'passed', 0::bigint, 'passed', CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp), CAST(TIMESTAMP WITH TIME ZONE '2015-02-12 21:26:48.943438+00' as cd_timestamp));

  RETURN QUERY SELECT results_eq(
    format('EXECUTE stats(%L)', 'BigCo'),
    'bigco_stats9',
    'Should retrieve the correct pipeline stats for BigCo');

END;
$$;

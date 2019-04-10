CREATE OR REPLACE FUNCTION test_stage_runs_to_restart_view()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  PREPARE stages AS
  VALUES (924::bigint, 'f72bd410-e7e0-4a38-a28f-4365013f0858'::uuid, 'verify', 'idle', false),
         (1767::bigint, 'e4253815-3577-40fa-b08d-edfe3782eddd'::uuid, 'acceptance', 'running', false),
         (4740::bigint, '8dd41ad4-0055-41ca-b288-65968c4940b7'::uuid, 'delivered', 'running', false);

  PREPARE restart AS
    SELECT * FROM stage_runs_to_restart order by id;

  RETURN QUERY SELECT results_eq(
    'EXECUTE restart',
    'stages',
    'Should retrieve the correct stage_runs to restart.');

  -- Lets follow the verity change through and verify each stop along the way.
  INSERT INTO stage_runs(change_id, stage, status, finished)
  VALUES ('f72bd410-e7e0-4a38-a28f-4365013f0858', 'verify', 'running', false);

  PREPARE stages2 AS
  VALUES (1767::bigint, 'e4253815-3577-40fa-b08d-edfe3782eddd'::uuid, 'acceptance', 'running', false),
         (4740::bigint, '8dd41ad4-0055-41ca-b288-65968c4940b7'::uuid, 'delivered', 'running', false),
         (4775::bigint, 'f72bd410-e7e0-4a38-a28f-4365013f0858'::uuid, 'verify', 'running', false);

  RETURN QUERY SELECT results_eq(
    'EXECUTE restart',
    'stages2',
    'Should retrieve the correct stage_runs to restart.');

  UPDATE stage_runs SET status='passed', finished=true WHERE id=4784;
  INSERT INTO stage_runs(change_id, stage, status, finished)
  VALUES ('f72bd410-e7e0-4a38-a28f-4365013f0858', 'build', 'running', false);

  PREPARE stages3 AS
  VALUES (1767::bigint, 'e4253815-3577-40fa-b08d-edfe3782eddd'::uuid, 'acceptance', 'running', false),
         (4740::bigint, '8dd41ad4-0055-41ca-b288-65968c4940b7'::uuid, 'delivered', 'running', false),
         (4776::bigint, 'f72bd410-e7e0-4a38-a28f-4365013f0858'::uuid, 'build', 'running', false);

  RETURN QUERY SELECT results_eq(
    'EXECUTE restart',
    'stages3',
    'Should retrieve the correct stage_runs to restart.');

  UPDATE stage_runs SET status='passed', finished=true WHERE id=4785;
  INSERT INTO stage_runs(change_id, stage, status, finished)
  VALUES ('f72bd410-e7e0-4a38-a28f-4365013f0858', 'acceptance', 'running', false);

  PREPARE stages4 AS
  VALUES (1767::bigint, 'e4253815-3577-40fa-b08d-edfe3782eddd'::uuid, 'acceptance', 'running', false),
         (4740::bigint, '8dd41ad4-0055-41ca-b288-65968c4940b7'::uuid, 'delivered', 'running', false),
         (4777::bigint, 'f72bd410-e7e0-4a38-a28f-4365013f0858'::uuid, 'acceptance', 'running', false);

  RETURN QUERY SELECT results_eq(
    'EXECUTE restart',
    'stages4',
    'Should retrieve the correct stage_runs to restart.');

  UPDATE stage_runs SET status='passed', finished=true WHERE id=4746;
  INSERT INTO stage_runs(change_id, stage, status, finished)
  VALUES ('f72bd410-e7e0-4a38-a28f-4365013f0858', 'union', 'running', false);

  PREPARE stages5 AS
  VALUES (1767::bigint, 'e4253815-3577-40fa-b08d-edfe3782eddd'::uuid, 'acceptance', 'running', false),
         (4740::bigint, '8dd41ad4-0055-41ca-b288-65968c4940b7'::uuid, 'delivered', 'running', false),
         (4778::bigint, 'f72bd410-e7e0-4a38-a28f-4365013f0858'::uuid, 'union', 'running', false);

  RETURN QUERY SELECT results_eq(
    'EXECUTE restart',
    'stages5',
    'Should retrieve the correct stage_runs to restart.');

  UPDATE stage_runs SET status='passed', finished=true WHERE id=4747;
  INSERT INTO stage_runs(change_id, stage, status, finished)
  VALUES ('f72bd410-e7e0-4a38-a28f-4365013f0858', 'rehearsal', 'running', false);

  PREPARE stages6 AS
  VALUES (1767::bigint, 'e4253815-3577-40fa-b08d-edfe3782eddd'::uuid, 'acceptance', 'running', false),
         (4740::bigint, '8dd41ad4-0055-41ca-b288-65968c4940b7'::uuid, 'delivered', 'running', false),
         (4779::bigint, 'f72bd410-e7e0-4a38-a28f-4365013f0858'::uuid, 'rehearsal', 'running', false);

  RETURN QUERY SELECT results_eq(
    'EXECUTE restart',
    'stages6',
    'Should retrieve the correct stage_runs to restart.');

  UPDATE stage_runs SET status='passed', finished=true WHERE id=4740;
  INSERT INTO stage_runs(change_id, stage, status, finished)
  VALUES ('f72bd410-e7e0-4a38-a28f-4365013f0858', 'delivered', 'running', false);

  UPDATE stage_runs SET status='passed', finished=true WHERE id=4748;
  INSERT INTO stage_runs(change_id, stage, status, finished)
  VALUES ('f72bd410-e7e0-4a38-a28f-4365013f0858', 'delivered', 'running', false);

  PREPARE stages7 AS
  VALUES (1767::bigint, 'e4253815-3577-40fa-b08d-edfe3782eddd'::uuid, 'acceptance', 'running', false),
         (4781::bigint, 'f72bd410-e7e0-4a38-a28f-4365013f0858'::uuid, 'delivered', 'running', false);

  RETURN QUERY SELECT results_eq(
    'EXECUTE restart',
    'stages7',
    'Should retrieve the correct stage_runs to restart.');

END;
$$;

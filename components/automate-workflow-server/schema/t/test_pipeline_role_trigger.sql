CREATE OR REPLACE FUNCTION test_pipeline_role_trigger()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  PREPARE happy_path AS
    INSERT INTO pipeline_user_roles(pipeline_id, user_id, role)
    VALUES (pipe('BigCo', 'BigCo Engineering', 'skunkworks', 'master'), u('BigCo', 'BigCo User'), 'admin');
  RETURN QUERY SELECT lives_ok(
    'happy_path',
    'Can make users in the pipeline admins');

  PREPARE user_not_in_pipeline AS
    INSERT INTO pipeline_user_roles(pipeline_id, user_id, role)
    VALUES (pipe('BigCo', 'BigCo Engineering', 'skunkworks', 'master'), u('SmallCo', 'SmallCo User'), 'admin');
  RETURN QUERY SELECT throws_ok(
    'user_not_in_pipeline',
    'CD001',
    'Membership prerequisite for role granting not met',
    'Cannot make a user from another enterprise a pipeline admin');

  PREPARE null_user AS
    INSERT INTO pipeline_user_roles(pipeline_id, user_id, role)
    VALUES (pipe('BigCo', 'BigCo Engineering', 'skunkworks', 'master'), NULL, 'admin');
  RETURN QUERY SELECT throws_ok(
    'null_user',
    'null value in column "user_id" violates not-null constraint',
    'Cannot insert a NULL user; trigger falls back to NOT NULL constraint on table');

  PREPARE null_pipeline AS
    INSERT INTO pipeline_user_roles(pipeline_id, user_id, role)
    VALUES (NULL, u('BigCo', 'BigCo User'), 'admin');
  RETURN QUERY SELECT throws_ok(
    'null_pipeline',
    'null value in column "pipeline_id" violates not-null constraint',
    'Cannot insert a NULL pipeline; trigger falls back to NOT NULL constraint on table');

  PREPARE non_existent_user AS
    INSERT INTO pipeline_user_roles(pipeline_id, user_id, role)
    VALUES (pipe('BigCo', 'BigCo Engineering', 'skunkworks', 'master'), 123456789, 'admin');
  RETURN QUERY SELECT throws_ok(
    'non_existent_user',
    'insert or update on table "pipeline_user_roles" violates foreign key constraint "pipeline_user_roles_user_id_fkey"',
    'Cannot insert a non-existent user; trigger falls back to FK constraint on table');

  PREPARE non_existent_pipeline AS
    INSERT INTO pipeline_user_roles(pipeline_id, user_id, role)
    VALUES (123456789, u('BigCo', 'BigCo User'), 'admin');
  RETURN QUERY SELECT throws_ok(
    'non_existent_pipeline',
    'insert or update on table "pipeline_user_roles" violates foreign key constraint "pipeline_user_roles_pipeline_id_fkey"',
    'Cannot insert a non-existent pipeline; trigger falls back to FK constraint on table');

  RETURN;
END;
$$;

CREATE OR REPLACE FUNCTION test_project_role_trigger()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  PREPARE happy_path AS
    INSERT INTO project_user_roles(project_id, user_id, role)
    VALUES (proj('BigCo', 'BigCo Engineering', 'skunkworks'), u('BigCo', 'BigCo User'), 'admin');
  RETURN QUERY SELECT lives_ok(
    'happy_path',
    'Can make users in the project admins');

  PREPARE user_not_in_project AS
    INSERT INTO project_user_roles(project_id, user_id, role)
    VALUES (proj('BigCo', 'BigCo Engineering', 'skunkworks'), u('SmallCo', 'SmallCo User'), 'admin');
  RETURN QUERY SELECT throws_ok(
    'user_not_in_project',
    'CD001',
    'Membership prerequisite for role granting not met',
    'Cannot make a user from another enterprise a project admin');

  PREPARE null_user AS
    INSERT INTO project_user_roles(project_id, user_id, role)
    VALUES (proj('BigCo', 'BigCo Engineering', 'skunkworks'), NULL, 'admin');
  RETURN QUERY SELECT throws_ok(
    'null_user',
    'null value in column "user_id" violates not-null constraint',
    'Cannot insert a NULL user; trigger falls back to NOT NULL constraint on table');

  PREPARE null_project AS
    INSERT INTO project_user_roles(project_id, user_id, role)
    VALUES (NULL, u('BigCo', 'BigCo User'), 'admin');
  RETURN QUERY SELECT throws_ok(
    'null_project',
    'null value in column "project_id" violates not-null constraint',
    'Cannot insert a NULL project; trigger falls back to NOT NULL constraint on table');

  PREPARE non_existent_user AS
    INSERT INTO project_user_roles(project_id, user_id, role)
    VALUES (proj('BigCo', 'BigCo Engineering', 'skunkworks'), 123456789, 'admin');
  RETURN QUERY SELECT throws_ok(
    'non_existent_user',
    'insert or update on table "project_user_roles" violates foreign key constraint "project_user_roles_user_id_fkey"',
    'Cannot insert a non-existent user; trigger falls back to FK constraint on table');

  PREPARE non_existent_project AS
    INSERT INTO project_user_roles(project_id, user_id, role)
    VALUES (123456789, u('BigCo', 'BigCo User'), 'admin');
  RETURN QUERY SELECT throws_ok(
    'non_existent_project',
    'insert or update on table "project_user_roles" violates foreign key constraint "project_user_roles_project_id_fkey"',
    'Cannot insert a non-existent project; trigger falls back to FK constraint on table');

  RETURN;
END;
$$;

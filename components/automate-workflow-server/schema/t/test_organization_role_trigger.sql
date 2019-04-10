CREATE OR REPLACE FUNCTION test_organization_role_trigger()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  PREPARE happy_path AS
    INSERT INTO organization_user_roles(organization_id, user_id, role)
    VALUES (org('BigCo', 'BigCo Engineering'), u('BigCo', 'BigCo User'), 'admin');
  RETURN QUERY SELECT lives_ok(
    'happy_path',
    'Can make users in the org admins');

  PREPARE user_not_in_organization AS
    INSERT INTO organization_user_roles(organization_id, user_id, role)
    VALUES (org('BigCo', 'BigCo Engineering'), u('SmallCo', 'SmallCo User'), 'admin');
  RETURN QUERY SELECT throws_ok(
    'user_not_in_organization',
    'CD001',
    'Membership prerequisite for role granting not met',
    'Cannot make a user from another org an admin');

  PREPARE null_user AS
    INSERT INTO organization_user_roles(organization_id, user_id, role)
    VALUES (org('BigCo', 'BigCo Engineering'), NULL, 'admin');
  RETURN QUERY SELECT throws_ok(
    'null_user',
    'null value in column "user_id" violates not-null constraint',
    'Cannot insert a NULL user; trigger falls back to NOT NULL constraint on table');

  PREPARE null_organization AS
    INSERT INTO organization_user_roles(organization_id, user_id, role)
    VALUES (NULL, u('BigCo', 'BigCo User'), 'admin');
  RETURN QUERY SELECT throws_ok(
    'null_organization',
    'null value in column "organization_id" violates not-null constraint',
    'Cannot insert a NULL organization; trigger falls back to NOT NULL constraint on table');

  PREPARE non_existent_user AS
    INSERT INTO organization_user_roles(organization_id, user_id, role)
    VALUES (org('BigCo', 'BigCo Engineering'), 123456789, 'admin');
  RETURN QUERY SELECT throws_ok(
    'non_existent_user',
    'insert or update on table "organization_user_roles" violates foreign key constraint "organization_user_roles_user_id_fkey"',
    'Cannot insert a non-existent user; trigger falls back to FK constraint on table');

  PREPARE non_existent_organization AS
    INSERT INTO organization_user_roles(organization_id, user_id, role)
    VALUES (123456789, u('BigCo', 'BigCo User'), 'admin');
  RETURN QUERY SELECT throws_ok(
    'non_existent_organization',
    'insert or update on table "organization_user_roles" violates foreign key constraint "organization_user_roles_organization_id_fkey"',
    'Cannot insert a non-existent organization; trigger falls back to FK constraint on table');

  RETURN;
END;
$$;

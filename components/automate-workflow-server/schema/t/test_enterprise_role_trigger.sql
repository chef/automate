CREATE OR REPLACE FUNCTION test_enterprise_role_trigger()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  PREPARE happy_path AS
    INSERT INTO enterprise_user_roles(enterprise_id, user_id, role)
    VALUES (ent('BigCo'), u('BigCo', 'BigCo User'), 'admin');
  RETURN QUERY SELECT lives_ok(
    'happy_path',
    'Can grant users a role in their enterprise');

  PREPARE user_not_in_enterprise AS
    INSERT INTO enterprise_user_roles(enterprise_id, user_id, role)
    VALUES (ent('BigCo'), u('SmallCo', 'SmallCo User'), 'admin');
  RETURN QUERY SELECT throws_ok(
    'user_not_in_enterprise',
    'CD001',
    'Membership prerequisite for role granting not met',
    'Cannot grant users a role in a different enterprise');

  PREPARE null_user AS
    INSERT INTO enterprise_user_roles(enterprise_id, user_id, role)
    VALUES (ent('BigCo'), NULL, 'admin');
  RETURN QUERY SELECT throws_ok(
    'null_user',
    'null value in column "user_id" violates not-null constraint',
    'Cannot insert a NULL user; trigger falls back to NOT NULL constraint on table');

  PREPARE null_enterprise AS
    INSERT INTO enterprise_user_roles(enterprise_id, user_id, role)
    VALUES (NULL, u('BigCo', 'BigCo User'), 'admin');
  RETURN QUERY SELECT throws_ok(
    'null_enterprise',
    'null value in column "enterprise_id" violates not-null constraint',
    'Cannot insert a NULL enterprise; trigger falls back to NOT NULL constraint on table');

  PREPARE non_existent_user AS
    INSERT INTO enterprise_user_roles(enterprise_id, user_id, role)
    VALUES (ent('BigCo'), 123456789, 'admin');
  RETURN QUERY SELECT throws_ok(
    'non_existent_user',
    'insert or update on table "enterprise_user_roles" violates foreign key constraint "enterprise_user_roles_user_id_fkey"',
    'Cannot insert a non-existent user; trigger falls back to FK constraint on table');

  PREPARE non_existent_enterprise AS
    INSERT INTO enterprise_user_roles(enterprise_id, user_id, role)
    VALUES (123456789, u('BigCo', 'BigCo User'), 'admin');
  RETURN QUERY SELECT throws_ok(
    'non_existent_enterprise',
    'insert or update on table "enterprise_user_roles" violates foreign key constraint "enterprise_user_roles_enterprise_id_fkey"',
    'Cannot insert a non-existent enterprise; trigger falls back to FK constraint on table');

  RETURN;
END;
$$;

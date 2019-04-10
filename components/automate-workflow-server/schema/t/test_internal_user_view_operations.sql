CREATE OR REPLACE FUNCTION test_internal_user_view_operations()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
DECLARE
  test_user_name CONSTANT internal_users.name%TYPE = 'doge';
  test_enterprise_id CONSTANT internal_users.enterprise_id%TYPE = ent('BigCo');
  test_email CONSTANT internal_users.email%TYPE = 'doge@bigco.com';
  test_hashed_pass CONSTANT internal_users.hashed_pass%TYPE = 'pretendthisishashed';
  updated_test_hashed_pass CONSTANT internal_users.hashed_pass%TYPE = 'pretendthisis-really-hashed';
  the_row internal_users%ROWTYPE;
BEGIN
  PERFORM *
  FROM internal_users
  WHERE name = test_user_name;

  RETURN QUERY SELECT is(
    FOUND,
    false,
    'Verify the test user "' || test_user_name || '" is not already in the database at the beginning of the test');

  INSERT INTO internal_users(enterprise_id, name)
  VALUES (test_enterprise_id, test_user_name)
  RETURNING * INTO the_row;

  PREPARE get_test_internal_user AS
  SELECT * FROM users
  WHERE enterprise_id = $1
    AND name = $2
    AND user_type = 'internal';

  PREPARE get_test_internal_user_password AS
  SELECT * FROM user_passwords
  WHERE id = $1;

  RETURN QUERY SELECT row_eq(
    format('EXECUTE get_test_internal_user(%s, %L)', test_enterprise_id, test_user_name),
    ROW(the_row.id, test_enterprise_id, test_user_name, NULL, NULL, NULL, NULL, 'internal', NULL)::users,
    'Inserting a minimal internal user should create a record in the users table');

  RETURN QUERY SELECT row_eq(
    format('EXECUTE get_test_internal_user_password(%s)', the_row.id),
    NULL::user_passwords,
    'Inserting a minimal internal user without a password should NOT create a record in the user_passwords table');

  UPDATE internal_users
     SET email = test_email
   WHERE id = the_row.id;

  RETURN QUERY SELECT row_eq(
    format('EXECUTE get_test_internal_user(%s, %L)', test_enterprise_id, test_user_name),
    ROW(the_row.id, test_enterprise_id, test_user_name, NULL, NULL, NULL, test_email, 'internal', NULL)::users,
    'Updating an internal user should update the underlying row in the users table');

  RETURN QUERY SELECT row_eq(
    format('EXECUTE get_test_internal_user_password(%s)', the_row.id),
    NULL::user_passwords,
    'Updating an internal user, but not setting the password should still NOT create a record in the user_passwords table');

  UPDATE internal_users
     SET hashed_pass = test_hashed_pass,
         hash_type = 'bcrypt'
   WHERE id = the_row.id;

  RETURN QUERY SELECT row_eq(
    format('EXECUTE get_test_internal_user(%s, %L)', test_enterprise_id, test_user_name),
    ROW(the_row.id, test_enterprise_id, test_user_name, NULL, NULL, NULL, test_email, 'internal', NULL)::users,
    'Updating an internal user should update the underlying row in the users table');

  RETURN QUERY SELECT row_eq(
    format('EXECUTE get_test_internal_user_password(%s)', the_row.id),
    ROW(the_row.id, test_hashed_pass, 'bcrypt')::user_passwords,
    'Updating an internal user by adding a password should create a record in the user_passwords table');

  UPDATE internal_users
     SET hashed_pass = updated_test_hashed_pass
   WHERE id = the_row.id;

  RETURN QUERY SELECT row_eq(
    format('EXECUTE get_test_internal_user(%s, %L)', test_enterprise_id, test_user_name),
    ROW(the_row.id, test_enterprise_id, test_user_name, NULL, NULL, NULL, test_email, 'internal', NULL)::users,
    'Updating an internal user should update the underlying row in the users table');

  RETURN QUERY SELECT row_eq(
    format('EXECUTE get_test_internal_user_password(%s)', the_row.id),
    ROW(the_row.id, updated_test_hashed_pass, 'bcrypt')::user_passwords,
    'Updating an internal user by adding a password should create a record in the user_passwords table');

  UPDATE internal_users
     SET hashed_pass = NULL,
         hash_type = NULL
   WHERE id = the_row.id;

  RETURN QUERY SELECT row_eq(
    format('EXECUTE get_test_internal_user(%s, %L)', test_enterprise_id, test_user_name),
    ROW(the_row.id, test_enterprise_id, test_user_name, NULL, NULL, NULL, test_email, 'internal', NULL)::users,
    'Updating an internal user should update the underlying row in the users table');

  RETURN QUERY SELECT row_eq(
    format('EXECUTE get_test_internal_user_password(%s)', the_row.id),
    NULL::user_passwords,
    'Updating an internal user by removing the password should remove the corresponding record in the user_passwords table');

  UPDATE internal_users
     SET hashed_pass = test_hashed_pass,
         hash_type = 'bcrypt'
   WHERE id = the_row.id;

  RETURN QUERY SELECT row_eq(
    format('EXECUTE get_test_internal_user_password(%s)', the_row.id),
    ROW(the_row.id, test_hashed_pass, 'bcrypt')::user_passwords,
    'Just adding a password back in preparation for DELETE testing of internal_users');

  DELETE FROM internal_users
  WHERE id = the_row.id;

  RETURN QUERY SELECT row_eq(
    format('EXECUTE get_test_internal_user(%s, %L)', test_enterprise_id, test_user_name),
    NULL::users,
    'Deleting an internal user should delete the underlying row in the users table');

  -- This part should actually be handled by cascading FK constraints
  -- and not the trigger directly, but it never hurts to verify
  RETURN QUERY SELECT row_eq(
    format('EXECUTE get_test_internal_user_password(%s)', the_row.id),
    NULL::user_passwords,
    'Deleting an internal user should also delete the underlying row in the user_passwords table (if it exists)');

END;
$$;

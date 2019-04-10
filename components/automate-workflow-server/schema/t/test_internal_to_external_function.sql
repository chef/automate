CREATE OR REPLACE FUNCTION test_internal_to_external_function()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
DECLARE
  v_user_id users.id%TYPE;
BEGIN

  WITH new_user AS (
    INSERT INTO users(enterprise_id, name, user_type)
    SELECT id, 'switchy', 'internal'
    FROM enterprises
    WHERE name = 'BigCo'
    RETURNING *),
  new_password AS (
    INSERT INTO user_passwords(id, hashed_pass, hash_type)
    SELECT id, 'pretendthisishashed', 'bcrypt'
    FROM new_user
    RETURNING *)
  SELECT id FROM new_user INTO v_user_id;

  RETURN QUERY
  SELECT results_eq(
    format('SELECT name FROM users WHERE id = %s AND user_type = %L', v_user_id, 'internal'),
    ARRAY['switchy'],
    'Created a new internal user for this test');

  RETURN QUERY
  SELECT results_eq(
    format('SELECT id FROM user_passwords WHERE id = %s', v_user_id),
    ARRAY[v_user_id],
    'Created a new password for the user for this test');

  RETURN QUERY
  SELECT lives_ok(
    format('SELECT utility.internal_to_external(%L, %L)', 'BigCo', 'switchy'),
    'Can run the internal_to_external function on the test user');

  RETURN QUERY
  SELECT results_eq(
    format('SELECT name FROM users WHERE id = %s AND user_type = %L', v_user_id, 'external'),
    ARRAY['switchy'],
    'The test user is now an external user');

  RETURN QUERY
  SELECT results_eq(
    format('SELECT id FROM user_passwords WHERE id = %s', v_user_id),
    ARRAY[]::bigint[],
    'No more password for external test user');

  -- Error Checking

  RETURN QUERY
  SELECT throws_ok(
    format('SELECT utility.internal_to_external(%L, %L)', 'LolWut', 'switchy'),
    'CD003',
    'Enterprise not found',
    'Throws an error if trying to flip a user in a non-existent enterprise');

  RETURN QUERY
  SELECT throws_ok(
    format('SELECT utility.internal_to_external(%L, %L)', 'BigCo', 'who_is_this_user_i_do_not_think_they_exist'),
    'CD004',
    'User not found',
    'Throws an error if trying to flip a non-existent user');

  RETURN QUERY
  SELECT throws_ok(
    format('SELECT utility.internal_to_external(%L, %L)', 'BigCo', 'admin'),
    'P0001',
    'Cannot change user type for special users',
    'Cannot switch the "admin" user');

  RETURN QUERY
  SELECT throws_ok(
    format('SELECT utility.internal_to_external(%L, %L)', 'BigCo', 'builder'),
    'P0001',
    'Cannot change user type for special users',
    'Cannot switch the "builder" user');

END;
$$;

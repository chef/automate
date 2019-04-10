CREATE OR REPLACE FUNCTION test_update_token_birthday_trigger()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
DECLARE

  -- If things die here, then the most likely cause is that the test
  -- data has changed such that the assumptions here are no longer
  -- valid
  test_user_id users.id%TYPE NOT NULL = u('BigCo', 'BigCo User');

  --cd_timestamp can't be NULL!
  before cd_timestamp DEFAULT 'epoch';
  after cd_timestamp DEFAULT 'epoch';
BEGIN

  SELECT auth_token_bday
  FROM user_tokens
  WHERE id = test_user_id
  INTO before;

  UPDATE user_tokens
  SET auth_token_bday = now() - interval '1 week'
  WHERE id = test_user_id
  RETURNING auth_token_bday
  INTO after;

  RETURN QUERY SELECT is(
    after,
    before,
    'Updating the token''s birthday manually is not allowed');

  UPDATE user_tokens
  SET auth_token = 'fake_bigco_token' -- This is the default token for this user in the test data
  WHERE id = test_user_id
  RETURNING auth_token_bday
  INTO after;

  RETURN QUERY SELECT is(
    after,
    before,
    'Updating a user, but setting the token to the same value does not affect the token birthdate; you actually need to change the token!');

  UPDATE user_tokens
  SET auth_token = 'behold_a_brand_spanking_new_token' -- This is NOT the default token for this user in the test data
  WHERE id = test_user_id
  RETURNING auth_token_bday
  INTO after;

  RETURN QUERY SELECT ok(before < after,
  'Updating a user and changing its token automatically updates the token birthdate');

END;
$$;

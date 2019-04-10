CREATE OR REPLACE FUNCTION test_update_project_guid()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
DECLARE

  -- If things die here, then the most likely cause is that the test
  -- data has changed such that the assumptions here are no longer
  -- valid
  test_proj_id projects.id%TYPE NOT NULL = proj('BigCo', 'BigCo Engineering', 'skunkworks');

  before UUID;
  after UUID;

BEGIN

  SELECT guid
  FROM projects
  WHERE id = test_proj_id
  INTO before;

  UPDATE projects
  SET guid = uuid_generate_v4()
  WHERE id = test_proj_id
  RETURNING guid
  INTO after;

  RETURN QUERY SELECT is(
    after,
    before,
    'A project''s GUID can never be changed');

END;
$$;

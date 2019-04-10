CREATE OR REPLACE FUNCTION test_update_comment()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
DECLARE
  test_enterprise CONSTANT enterprises.name%TYPE = 'BigCo';
  test_user CONSTANT users.name%TYPE = 'BigCo Chaos Monkey';
  test_user_id CONSTANT users.id%TYPE = u(test_enterprise, test_user);
  test_another_user_id CONSTANT users.id%TYPE = u(test_enterprise, 'BigCo User');
  test_organization CONSTANT organizations.name%TYPE = 'BigCo Engineering';
  test_project CONSTANT projects.name%TYPE = 'skunkworks';
  test_pipeline CONSTANT pipelines.name%TYPE = 'master';
  test_feature_branch CONSTANT changes.feature_branch%TYPE = 'bc_eng/test_update_comments';

  test_patchset_id patchsets.id%TYPE;

  test_patchset_comment_id comments.id%TYPE;
  test_line_comment_id comments.id%TYPE;
  test_comment_comment_id comments.id%TYPE;

  timestamp_1 comments.last_modif_or_publication_timestamp%TYPE;
  timestamp_2 comments.last_modif_or_publication_timestamp%TYPE;
  timestamp_3 comments.last_modif_or_publication_timestamp%TYPE;
  timestamp_4 comments.last_modif_or_publication_timestamp%TYPE;
  timestamp_5 comments.last_modif_or_publication_timestamp%TYPE;
  timestamp_6 comments.last_modif_or_publication_timestamp%TYPE;
  timestamp_7 comments.last_modif_or_publication_timestamp%TYPE;
  timestamp_8 comments.last_modif_or_publication_timestamp%TYPE;
BEGIN
  -- first we need to create a new patchset
  SELECT id
  FROM create_patchset_and_change(test_enterprise, test_user, test_organization, test_project, test_pipeline, test_feature_branch, 'abcdef')
  INTO test_patchset_id;

  -- then a comment - any type will do, that's not what we're testing here
  -- and let's check the timestamp gets set to clock_timestamp
  SELECT NOW() INTO timestamp_1;
  INSERT INTO comments(patchset_id, submitter_id, content, type, status)
  VALUES(test_patchset_id, test_user_id, 'hey you', 'patchset', 'published')
  RETURNING id, last_modif_or_publication_timestamp
  INTO test_patchset_comment_id, timestamp_2;
  SELECT clock_timestamp() INTO timestamp_3;

  PREPARE assert_timestamps_ordered(cd_timestamp, cd_timestamp, cd_timestamp) AS
  SELECT ($1 <= $2) AND ($2 <= $3);

  RETURN QUERY SELECT results_eq(
    format('EXECUTE assert_timestamps_ordered(%L, %L, %L)', timestamp_1, timestamp_2, timestamp_3),
    ARRAY['true']::BOOLEAN[]
  );

  -- now we update the comment, which should update its timestamp
  UPDATE comments
  SET content = 'damn you'
  WHERE id = test_patchset_comment_id
  RETURNING last_modif_or_publication_timestamp
  INTO timestamp_4;
  SELECT clock_timestamp() INTO timestamp_5;

  RETURN QUERY SELECT results_eq(
    format('EXECUTE assert_timestamps_ordered(%L, %L, %L)', timestamp_3, timestamp_4, timestamp_5),
    ARRAY['true']::BOOLEAN[]
  );

  -- also, we shouldn't be able to update anything but the status and the content
  RETURN QUERY SELECT throws_ok(
    format('WITH another_patchset AS ('
           ' SELECT id FROM create_patchset_and_change(%L, %L, %L, %L, %L, ''another/feat/branch'', ''abcdef'')'
           ')'
           'UPDATE comments '
           'SET patchset_id = another_patchset.id '
           'FROM another_patchset '
           'WHERE comments.id = %L', test_enterprise, test_user, test_organization, test_project, test_pipeline, test_patchset_comment_id),
    'CD012',
    'Forbidden update on a draft comment',
    'Cannot update patchset_id on a draft comment'
  );
  RETURN QUERY SELECT throws_ok(
    format('UPDATE comments '
           'SET submitter_id = %L '
           'WHERE id = %L', test_another_user_id, test_patchset_comment_id),
    'CD012',
    'Forbidden update on a draft comment',
    'Cannot update submitter_id on a draft comment'
  );
  RETURN QUERY SELECT throws_ok(
    format('UPDATE comments '
           'SET type = ''line'' '
           'WHERE id = %L', test_patchset_comment_id),
    'CD012',
    'Forbidden update on a draft comment',
    'Cannot update type on a draft comment'
  );

  -- we need a line comment to test the following two
  INSERT INTO comments(patchset_id, submitter_id, content, type, status, line_range)
  VALUES(test_patchset_id, test_user_id, 'who? me?', 'line', 'draft', int4range(10, 20))
  RETURNING id
  INTO test_line_comment_id;

  RETURN QUERY SELECT throws_ok(
    format('UPDATE comments '
           'SET line_range = int4range(12, 20) '
           'WHERE id = %L', test_line_comment_id),
    'CD012',
    'Forbidden update on a draft comment',
    'Cannot update line_range on a draft comment'
  );
  RETURN QUERY SELECT throws_ok(
    format('UPDATE comments '
           'SET file_path = ''foo.java'' '
           'WHERE id = %L', test_line_comment_id),
    'CD012',
    'Forbidden update on a draft comment',
    'Cannot update file_path on a draft comment'
  );

  -- and finally we need a comment comment to test this one
  -- (perhaps the more important one)
  INSERT INTO comments(patchset_id, submitter_id, content, type, status, parent_id)
  VALUES(test_patchset_id, test_user_id, 'yes! you!', 'comment', 'draft', test_line_comment_id)
  RETURNING id
  INTO test_comment_comment_id;
  RETURN QUERY SELECT throws_ok(
    format('UPDATE comments '
           'SET parent_id = %L '
           'WHERE id = %L', test_patchset_id, test_comment_comment_id),
    'CD012',
    'Forbidden update on a draft comment',
    'Cannot update parent_id on a draft comment'
  );

  -- now we mark the patchset comment as published, which should still update its timestamp
  UPDATE comments
  SET status = 'published'
  WHERE id = test_patchset_comment_id
  RETURNING last_modif_or_publication_timestamp
  INTO timestamp_6;
  SELECT clock_timestamp() INTO timestamp_7;

  RETURN QUERY SELECT results_eq(
    format('EXECUTE assert_timestamps_ordered(%L, %L, %L)', timestamp_5, timestamp_6, timestamp_7),
    ARRAY['true']::BOOLEAN[]
  );

END;
$$;

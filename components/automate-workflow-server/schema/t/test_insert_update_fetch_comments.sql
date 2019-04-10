CREATE OR REPLACE FUNCTION test_insert_update_fetch_comments()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
DECLARE
  test_enterprise CONSTANT enterprises.name%TYPE = 'BigCo';
  test_user CONSTANT users.name%TYPE = 'BigCo Chaos Monkey';
  test_user_id CONSTANT users.id%TYPE = u(test_enterprise, test_user);
  test_organization CONSTANT organizations.name%TYPE = 'BigCo Engineering';
  test_project CONSTANT projects.name%TYPE = 'skunkworks';
  test_pipeline CONSTANT pipelines.name%TYPE = 'master';
  test_feature_branch CONSTANT changes.feature_branch%TYPE = 'bc_eng/test_insert_update_fetch_comments';

  test_patchset_id patchsets.id%TYPE;
  test_change_id changes.id%TYPE;
  test_another_change_id changes.id%TYPE;

  test_comment_id_1 comments.id%TYPE;
  test_comment_id_2 comments.id%TYPE;
  test_comment_id_3 comments.id%TYPE;
  test_comment_id_4 comments.id%TYPE;
  test_comment_id_5 comments.id%TYPE;
  test_comment_id_6 comments.id%TYPE;
BEGIN
  -- first we need to create a new patchset
  SELECT id, change_id
  FROM create_patchset_and_change(test_enterprise, test_user, test_organization, test_project, test_pipeline, test_feature_branch, 'abcdef')
  INTO test_patchset_id, test_change_id;

  -- then we create a few comments on that patchset, of all types
  SELECT id
  FROM insert_or_update_comment(test_enterprise, test_user, test_change_id, 1::SMALLINT, NULL, 'patchset', 'patchset comment', NULL, NULL, NULL, NULL)
  INTO test_comment_id_1;
  SELECT id
  FROM insert_or_update_comment(test_enterprise, test_user, test_change_id, 1::SMALLINT, NULL, 'comment', 'reply to patchset comment', NULL, test_comment_id_1, NULL, NULL)
  INTO test_comment_id_2;
  SELECT id
  FROM insert_or_update_comment(test_enterprise, test_user, test_change_id, 1::SMALLINT, NULL, 'line', 'line comment on commit message', int4range(1, 2), NULL, NULL, NULL)
  INTO test_comment_id_3;
  SELECT id
  FROM insert_or_update_comment(test_enterprise, test_user, test_change_id, 1::SMALLINT, NULL, 'line', 'line comment on a regular file', int4range(1, 20), NULL, NULL, 'path/to/file.java')
  INTO test_comment_id_4;
  SELECT id
  FROM insert_or_update_comment(test_enterprise, test_user, test_change_id, 1::SMALLINT, NULL, 'comment', 'reply to line comment', NULL, test_comment_id_3, NULL, NULL)
  INTO test_comment_id_5;

  -- let's check the right patchset_id was set
  RETURN QUERY SELECT results_eq(
    format('SELECT count(1)
            FROM comments
            WHERE id = ANY(ARRAY[%L, %L, %L, %L, %L]::BIGINT[])
            AND patchset_id = %L',
           test_comment_id_1, test_comment_id_2, test_comment_id_3, test_comment_id_4, test_comment_id_5,
           test_patchset_id),
    ARRAY[5]::BIGINT[]
  );

  -- let's try updating
  RETURN QUERY SELECT results_eq(
    format('SELECT content
            FROM insert_or_update_comment(%L, %L, %L, 1::SMALLINT, %L, NULL, ''updated content'', NULL, NULL, NULL, NULL)',
           test_enterprise, test_user, test_change_id, test_comment_id_1),
    ARRAY['updated content']
  );

  -- okay, now to fetching
  -- all comments in the patchset...
  RETURN QUERY SELECT results_eq(
    format('SELECT id
            FROM fetch_comments(%L, 1::SMALLINT, NULL, NULL, NULL, ''false'')
            ORDER BY id',
           test_change_id),
    ARRAY[test_comment_id_1, test_comment_id_2, test_comment_id_3, test_comment_id_4, test_comment_id_5]
  );

  -- then only patchset comments and their replies...
  RETURN QUERY SELECT results_eq(
    format('SELECT id
            FROM fetch_comments(%L, 1::SMALLINT, ''patchset'', NULL, NULL, ''false'')
            ORDER BY id',
           test_change_id),
    ARRAY[test_comment_id_1, test_comment_id_2]
  );

  -- then only line comments and their replies...
  RETURN QUERY SELECT results_eq(
    format('SELECT id
            FROM fetch_comments(%L, 1::SMALLINT, ''line'', NULL, NULL, ''false'')
            ORDER BY id',
           test_change_id),
    ARRAY[test_comment_id_3, test_comment_id_4, test_comment_id_5]
  );

  -- then only line comments on the commit message and their replies...
  RETURN QUERY SELECT results_eq(
    format('SELECT id
            FROM fetch_comments(%L, 1::SMALLINT, ''line'', NULL, NULL, ''true'')
            ORDER BY id',
           test_change_id),
    ARRAY[test_comment_id_3, test_comment_id_5]
  );

  -- then only line comments on a given file and their replies...
  RETURN QUERY SELECT results_eq(
    format('SELECT id
            FROM fetch_comments(%L, 1::SMALLINT, ''line'', ''path/to/file.java'', NULL, ''false'')
            ORDER BY id',
           test_change_id),
    ARRAY[test_comment_id_4]
  );

  -- and finally a single comment!
  RETURN QUERY SELECT results_eq(
    format('SELECT id
            FROM fetch_comments(%L, NULL, NULL, NULL, %L, ''false'')
            ORDER BY id',
           test_change_id, test_comment_id_4),
    ARRAY[test_comment_id_4]
  );

  -- now a few pathological cases: inserting a comment comment
  -- with an invalid parent_id should yield a nice explicit exception
  DELETE FROM comments
  WHERE id = test_comment_id_1;
  RETURN QUERY SELECT throws_ok(
    format('SELECT *
            FROM insert_or_update_comment(%L, %L, %L, 1::SMALLINT, NULL, ''comment'', ''reply to patchset comment'', NULL, %L, NULL, NULL)',
           test_enterprise, test_user, test_change_id, test_comment_id_1),
    'CD014',
    'Unknown parent for a comment comment'
  );

  -- check that we can't update comments on another change
  SELECT change_id
  FROM create_patchset_and_change(test_enterprise, test_user, test_organization, test_project, test_pipeline, test_feature_branch || '_bis', 'abc123')
  INTO test_another_change_id;
  SELECT id
  FROM insert_or_update_comment(test_enterprise, test_user, test_another_change_id, 1::SMALLINT, NULL, 'patchset', 'patchset comment on another change', NULL, NULL, NULL, NULL)
  INTO test_comment_id_6;
  RETURN QUERY SELECT throws_ok(
    format('SELECT *
            FROM insert_or_update_comment(%L, %L, %L, 1::SMALLINT, %L, NULL, ''another content'', NULL, NULL, NULL, NULL)',
           test_enterprise, test_user, test_change_id, test_comment_id_6),
    'CD015',
    'Unrelated change and patchset'
  );
  -- for good measure let's make sure it has indeed not been updated
  RETURN QUERY SELECT results_eq(
    format('SELECT content
            FROM comments
            WHERE id = %L',
           test_comment_id_6),
    ARRAY['patchset comment on another change']
  );
  -- we can't fetch comments from another change, either
  RETURN QUERY SELECT throws_ok(
    format('SELECT *
            FROM fetch_comments(%L, 1::SMALLINT, NULL, NULL, %L, NULL)',
           test_change_id, test_comment_id_6),
    'CD015',
    'Unrelated change and patchset'
  );
  -- and still the same idea let's check we can't insert a comment comment in reply
  -- to a comment on anoter change
  RETURN QUERY SELECT throws_ok(
    format('SELECT *
            FROM insert_or_update_comment(%L, %L, %L, 1::SMALLINT, NULL, ''comment'', ''blouh'', NULL, %L, NULL, NULL)',
           test_enterprise, test_user, test_change_id, test_comment_id_6),
    'CD015',
    'Unrelated change and patchset'
  );

  -- okay, now let's check that we get a nice explicit exception when
  -- giving a invalid sequence number
  RETURN QUERY SELECT throws_ok(
    format('SELECT *
            FROM fetch_comments(%L, 28::SMALLINT, NULL, NULL, NULL, NULL)',
           test_change_id, test_comment_id_6),
    'CD017',
    'Unknown sequence number'
  );
  -- and same when inserting
  RETURN QUERY SELECT throws_ok(
    format('SELECT *
            FROM insert_or_update_comment(%L, %L, %L, 28::SMALLINT, NULL, ''patchset'', ''blouh'', NULL, NULL, NULL, NULL)',
           test_enterprise, test_user, test_change_id),
    'CD017',
    'Unknown sequence number'
  );

  -- and finally let's check that trying to update an unknown comment
  -- throws an explicit exception
  DELETE
  FROM comments
  WHERE id = test_comment_id_1;
  RETURN QUERY SELECT throws_ok(
    format('SELECT *
            FROM insert_or_update_comment(%L, %L, %L, 1::SMALLINT, %L, NULL, NULL, NULL, NULL, NULL, NULL)',
           test_enterprise, test_user, test_change_id, test_comment_id_1),
    'CD016',
    'Unknown comment'
  );
END;
$$;

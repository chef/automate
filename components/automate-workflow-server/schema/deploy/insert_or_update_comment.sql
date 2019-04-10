-- Deploy insert_or_update_comment
-- requires: comments, assert_patchset_in_change

-- This function is meant to be called directly from the comment endpoints
-- for both POST and PUT methods
-- It does all the necessary checks and avoid race conditions (since we still
-- don't have transactions in sqerl)
-- Assumes the input data is valid, in the sense well formed
-- (this is handled on the endpoint)

-- Returns both the newly created/updated comment and its author

BEGIN;

CREATE OR REPLACE FUNCTION insert_or_update_comment(
  p_enterprise_name enterprises.name%TYPE,
  p_user_name users.name%TYPE,
  p_change_id changes.id%TYPE,
  p_patchset_sequence_number patchsets.sequence_number%TYPE,

  p_comment_id comments.id%TYPE,
  p_type comments.type%TYPE,
  p_content comments.content%TYPE,
  p_line_range comments.line_range%TYPE,
  p_parent_id comments.parent_id%TYPE,
  p_status comments.status%TYPE,
  p_file_path comments.file_path%TYPE
)
-- we only return what's needed to generate the comments' JSON
-- and we keep the keys' names consistent to what's in there
RETURNS TABLE(id comments.id%TYPE,
              content comments.content%TYPE,
              type comments.type%TYPE,
              status comments.status%TYPE,
              datetime comments.last_modif_or_publication_timestamp%TYPE,
              line_range comments.line_range%TYPE,
              file comments.file_path%TYPE,
              parent_id comments.parent_id%TYPE,

              user_name users.name%TYPE,
              user_first_name users.first_name%TYPE,
              user_last_name users.last_name%TYPE,

              patchset patchsets.sequence_number%TYPE)
LANGUAGE plpgsql
AS $$
DECLARE
  v_patchset_id comments.patchset_id%TYPE;

  v_user users%ROWTYPE;
  v_comment comments%ROWTYPE;
BEGIN

  -- we need to fetch the user's data
  WITH ids AS (
    SELECT * FROM to_ids(p_enterprise_name, p_user_name, NULL, NULL, NULL)
  )
  SELECT users.*
  FROM users, ids
  WHERE users.id = ids.user_id
  INTO v_user;

  IF p_comment_id IS NULL THEN
    -- means we're inserting
    p_status = COALESCE(p_status, 'draft');

    -- we need to retrieve the patchset's id
    IF p_type = 'comment' THEN
      -- we get it from the parent
      SELECT c.patchset_id
      FROM comments AS c
      WHERE c.id = p_parent_id
      INTO v_patchset_id;

      -- we could just let it crash downstream when trying to insert
      -- a comment comment with an invalid parent_id, but that would
      -- result in a less explicit error message
      IF NOT FOUND THEN
        RAISE EXCEPTION
          USING ERRCODE = 'CD014',
                MESSAGE = 'Unknown parent for a comment comment',
                DETAIL  = 'There is no comment ' || p_parent_id,
                HINT    = 'A comment comment must point to an existing comment';
      END IF;

      -- in that case, we must also check that this is indeed part of said change
      PERFORM 1
      FROM assert_patchset_in_change(v_patchset_id, p_change_id);
    ELSE
      -- not a comment comment; we get the patchset_id from the sequence number
      v_patchset_id = patchset_id_from_seq_number(p_patchset_sequence_number, p_change_id);
    END IF;

    INSERT INTO comments(patchset_id, submitter_id, content, type, status, line_range, file_path, parent_id)
    VALUES(v_patchset_id, v_user.id, p_content, p_type, p_status, p_line_range, p_file_path, p_parent_id)
    RETURNING comments.*
    INTO v_comment;
  ELSE
    -- we're updating!
    -- slightly inefficient to do the select then the update,
    -- but that allows to check the patchset belongs to the change
    -- before trying to update, and so it allows avoiding to give any
    -- information on any comment to someone making ill-formed requests
    SELECT c.patchset_id, p.sequence_number
    FROM comments AS c
    JOIN patchsets AS p
    ON p.id = c.patchset_id
    WHERE c.id = p_comment_id
    INTO v_patchset_id, p_patchset_sequence_number;

    IF NOT FOUND THEN
      RAISE EXCEPTION
        USING ERRCODE = 'CD016',
              MESSAGE = 'Unknown comment',
              DETAIL  = 'There is no comment ' || p_comment_id,
              HINT    = 'Don''t try to alter The Void.';
    END IF;

    PERFORM 1
    FROM assert_patchset_in_change(v_patchset_id, p_change_id);

    UPDATE comments AS c
    SET content = COALESCE(p_content, c.content),
    status = COALESCE(p_status, c.status)
    WHERE c.id = p_comment_id
    RETURNING c.*
    INTO v_comment;
  END IF;

  RETURN QUERY SELECT v_comment.id,
                      v_comment.content,
                      v_comment.type,
                      v_comment.status,
                      v_comment.last_modif_or_publication_timestamp,
                      v_comment.line_range,
                      v_comment.file_path,
                      v_comment.parent_id,

                      v_user.name,
                      v_user.first_name,
                      v_user.last_name,

                      p_patchset_sequence_number;
END;
$$;

COMMIT;

-- Deploy fetch_comments
-- requires: comments, assert_patchset_in_change

-- This function is meant to be called directly from the GET comment endpoint
-- It does all the necessary checks and avoid race conditions (since we still
-- don't have transactions in sqerl)
-- Assumes the input data is valid, in the sense well formed
-- (this is handled on the endpoint)

-- Returns a list of both the comments and their authors

BEGIN;

CREATE OR REPLACE FUNCTION fetch_comments(
  p_change_id changes.id%TYPE,
  p_patchset_sequence_number patchsets.sequence_number%TYPE,

  -- all of the others can be NULL
  p_filter_type comments.type%TYPE,
  p_file_path comments.file_path%TYPE,
  p_comment_id comments.id%TYPE,
  p_commit_msg boolean
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
LANGUAGE plpgsql STABLE
AS $$
DECLARE
  v_patchset_id comments.patchset_id%TYPE;
BEGIN

  IF p_comment_id IS NOT NULL THEN
    -- we just want a single comment, no need for a complicated query

    RETURN QUERY
    SELECT comments.id,
           comments.content,
           comments.type,
           comments.status,
           comments.last_modif_or_publication_timestamp,
           comments.line_range,
           comments.file_path,
           comments.parent_id,

           users.name,
           users.first_name,
           users.last_name,

           patchsets.sequence_number
    FROM comments,
         users,
         patchsets,
         -- we still need to check that it's part of the given change though
         assert_patchset_in_change(comments.patchset_id, p_change_id)
    WHERE comments.submitter_id = users.id
    AND comments.id = p_comment_id
    AND patchsets.id = comments.patchset_id;

  ELSE
    -- let's retrieve the patchset_id
    v_patchset_id = patchset_id_from_seq_number(p_patchset_sequence_number, p_change_id);

    IF p_filter_type IS NULL AND p_comment_id IS NULL AND NOT p_commit_msg THEN
      -- we basically want all comments for a given patchset
      -- here again, things are simple

      RETURN QUERY
      SELECT comments.id,
             comments.content,
             comments.type,
             comments.status,
             comments.last_modif_or_publication_timestamp,
             comments.line_range,
             comments.file_path,
             comments.parent_id,

             users.name,
             users.first_name,
             users.last_name,

             p_patchset_sequence_number
      FROM comments
      JOIN users
      ON comments.submitter_id = users.id
      WHERE comments.patchset_id = v_patchset_id;

    ELSE
      -- the fun stuff's happening here

      RETURN QUERY EXECUTE
      'WITH RECURSIVE comment_forest(comment_id,
                                     comment_content,
                                     comment_type,
                                     comment_status,
                                     comment_last_modif_or_publication_timestamp,
                                     comment_line_range,
                                     comment_file_path,
                                     comment_parent_id,

                                     user_name,
                                     user_first_name,
                                     user_last_name) AS (
        SELECT comments.id,
               comments.content,
               comments.type,
               comments.status,
               comments.last_modif_or_publication_timestamp,
               comments.line_range,
               comments.file_path,
               comments.parent_id,

               users.name,
               users.first_name,
               users.last_name
        FROM comments
        JOIN users
        ON comments.submitter_id = users.id
        WHERE comments.patchset_id = ' || v_patchset_id
        || CASE WHEN p_filter_type IS NOT NULL
                THEN ' AND comments.type = ' || quote_literal(p_filter_type)
                ELSE ''
           END
        || CASE WHEN p_file_path IS NOT NULL
                THEN ' AND comments.file_path = ' || quote_literal(p_file_path)
                ELSE ''
           END
        || CASE WHEN p_commit_msg
                THEN ' AND comments.file_path IS NULL'
                ELSE ''
           END
      || ' UNION ALL
        SELECT comments.id,
               comments.content,
               comments.type,
               comments.status,
               comments.last_modif_or_publication_timestamp,
               comments.line_range,
               comments.file_path,
               comments.parent_id,

               users.name,
               users.first_name,
               users.last_name
        FROM comments
        JOIN users
        ON comments.submitter_id = users.id
        JOIN comment_forest
        ON comment_forest.comment_id = comments.parent_id
        WHERE comments.type = ''comment''
      )
      SELECT *, ' || p_patchset_sequence_number || '::SMALLINT FROM comment_forest';

    END IF;
  END IF;
END;
$$;

COMMIT;

-- Revert fetch_comments

BEGIN;

DROP FUNCTION IF EXISTS fetch_comments(
  p_change_id changes.id%TYPE,
  p_patchset_sequence_number patchsets.sequence_number%TYPE,
  p_filter_type comments.type%TYPE,
  p_file_path comments.file_path%TYPE,
  p_comment_id comments.id%TYPE,
  p_commit_msg boolean
);

COMMIT;

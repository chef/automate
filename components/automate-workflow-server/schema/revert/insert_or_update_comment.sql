-- Revert insert_or_update_comment

BEGIN;

DROP FUNCTION IF EXISTS insert_or_update_comment(
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
);

COMMIT;

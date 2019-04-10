-- Revert update_comment_trigger

BEGIN;

DROP TRIGGER IF EXISTS update_comment
  ON comments;

COMMIT;

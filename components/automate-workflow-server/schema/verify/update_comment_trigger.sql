-- Verify insert_comment_comment_trigger

BEGIN;

SELECT 1/COUNT(*)
FROM information_schema.triggers
WHERE event_object_table = 'comments'
  AND trigger_name       = 'update_comment'
  AND action_timing      = 'BEFORE'
  AND event_manipulation = 'UPDATE'
  AND action_orientation = 'ROW'
  AND action_statement   = 'EXECUTE PROCEDURE update_comment()';

ROLLBACK;

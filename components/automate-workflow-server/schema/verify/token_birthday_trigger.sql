-- Verify token_birthday_trigger

BEGIN;

SELECT 1/COUNT(*)
FROM information_schema.triggers
WHERE event_object_table = 'user_tokens'
  AND trigger_name       = 'update_token_birthday'
  AND action_timing      = 'BEFORE'
  AND event_manipulation = 'INSERT'
  AND action_orientation = 'ROW'
  AND action_statement   = 'EXECUTE PROCEDURE update_token_birthday()';

SELECT 1/COUNT(*)
FROM information_schema.triggers
WHERE event_object_table = 'user_tokens'
  AND trigger_name       = 'update_token_birthday'
  AND action_timing      = 'BEFORE'
  AND event_manipulation = 'UPDATE'
  AND action_orientation = 'ROW'
  AND action_statement   = 'EXECUTE PROCEDURE update_token_birthday()';

ROLLBACK;

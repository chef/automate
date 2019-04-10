-- Revert token_birthday_trigger

BEGIN;

DROP TRIGGER IF EXISTS update_token_birthday
  ON user_tokens;

COMMIT;

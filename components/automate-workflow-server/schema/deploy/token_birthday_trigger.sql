-- Deploy token_birthday_trigger
-- requires: update_token_birthday

BEGIN;

CREATE TRIGGER update_token_birthday
BEFORE INSERT OR UPDATE OF auth_token, auth_token_bday
ON user_tokens
FOR EACH ROW
EXECUTE PROCEDURE update_token_birthday();

COMMIT;

-- Deploy internal_user_triggers

BEGIN;

CREATE TRIGGER insert_internal_user
INSTEAD OF INSERT ON internal_users
FOR EACH ROW
  EXECUTE PROCEDURE insert_internal_user();

CREATE TRIGGER update_internal_user
INSTEAD OF UPDATE ON internal_users
FOR EACH ROW
  EXECUTE PROCEDURE update_internal_user();

CREATE TRIGGER delete_internal_user
INSTEAD OF DELETE ON internal_users
FOR EACH ROW
  EXECUTE PROCEDURE delete_internal_user();

COMMIT;

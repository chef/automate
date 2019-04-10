-- Revert delivery:add_saml_to_user_type from pg

BEGIN;

-- We delete the users, but the user_type 'saml' is not revertable
DELETE FROM users
WHERE user_type = 'saml';

CREATE TYPE user_type_old as ENUM('internal', 'external');
-- Drop all things that depend on user_type
-- Drop trigger that prevents type switches
DROP TRIGGER prevent_type_switch on users;
-- Drop internal_users view
DROP VIEW internal_users;

ALTER TABLE users
  ALTER COLUMN user_type TYPE user_type_old USING (user_type::text::user_type_old);
DROP TYPE user_type;
ALTER TYPE user_type_old RENAME TO user_type;

-- Re-create trigger that prevents type switches
CREATE TRIGGER prevent_type_switch
BEFORE UPDATE
ON users
FOR EACH ROW
WHEN (NEW.user_type != OLD.user_type)
EXECUTE PROCEDURE raise_user_type_change_exception();
-- Re-create view
CREATE OR REPLACE VIEW internal_users AS
SELECT u.id,
       u.enterprise_id,
       u.name,
       u.ssh_pub_key,
       u.first_name,
       u.last_name,
       u.email,
       p.hashed_pass,
       p.hash_type
FROM users AS u
NATURAL LEFT OUTER JOIN user_passwords AS p
WHERE u.user_type = 'internal';

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

CREATE OR REPLACE VIEW user_aliases AS
  SELECT u.id,
         u.enterprise_id,
         u.name,
         u.ssh_pub_key,
         u.first_name,
         u.last_name,
         u.email,
         u.user_type,
         o.oauth_app_id,
         o.alias
    FROM users AS u
    JOIN oauth_user_aliases AS o
      ON o.user_id = u.id;

COMMENT ON VIEW user_aliases IS
'A view of all users and their various OAuth aliases.';

COMMIT;

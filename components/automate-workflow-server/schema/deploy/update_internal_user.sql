-- Deploy update_internal_user

BEGIN;

CREATE OR REPLACE FUNCTION update_internal_user()
RETURNS TRIGGER
LANGUAGE plpgsql
AS $$
DECLARE
  new_user_id users.id%TYPE;
BEGIN
  UPDATE users
    SET enterprise_id = NEW.enterprise_id,
        name          = NEW.name,
        ssh_pub_key   = NEW.ssh_pub_key,
        first_name    = NEW.first_name,
        last_name     = NEW.last_name,
        email         = NEW.email
    WHERE id = NEW.id
      AND user_type = 'internal';
  IF FOUND THEN
    IF (NEW.hashed_pass IS NULL AND NEW.hash_type IS NULL) AND
       (OLD.hashed_pass IS NOT NULL AND OLD.hash_type IS NOT NULL) THEN
      -- Though we could just unconditionally execute this DELETE even
      -- if no password currently exists, it's an easy check to do
      DELETE FROM user_passwords WHERE id = NEW.id;
      RETURN NEW;

    ELSIF (NEW.hashed_pass, NEW.hash_type) IS DISTINCT FROM
          (OLD.hashed_pass, OLD.hash_type) THEN
      UPDATE user_passwords
         SET hashed_pass = NEW.hashed_pass,
             hash_type   = NEW.hash_type
       WHERE id = NEW.id;

      IF NOT FOUND THEN
        -- Here, there wasn't an existing password that was updated,
        -- so that means we're actually adding one
        INSERT INTO user_passwords(id, hashed_pass, hash_type)
          VALUES(NEW.id, NEW.hashed_pass, NEW.hash_type);
      END IF;

      RETURN NEW;
    ELSE
       RETURN NEW;
    END IF;
  ELSE
    RETURN NULL;
  END IF;
END;
$$;

COMMIT;

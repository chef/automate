-- Deploy insert_internal_user

BEGIN;

CREATE OR REPLACE FUNCTION insert_internal_user()
RETURNS TRIGGER
LANGUAGE plpgsql
AS $$
DECLARE
  new_user_id users.id%TYPE;
BEGIN
  INSERT INTO users(enterprise_id, name, ssh_pub_key, first_name, last_name, email, user_type)
       VALUES (NEW.enterprise_id, NEW.name, NEW.ssh_pub_key, NEW.first_name, NEW.last_name, NEW.email, 'internal')
       RETURNING id INTO new_user_id;

  IF NEW.hashed_pass IS NOT NULL AND
     NEW.hash_type IS NOT NULL THEN
       INSERT INTO user_passwords(id, hashed_pass, hash_type)
            VALUES (new_user_id, NEW.hashed_pass, NEW.hash_type);
  END IF;
  -- Always return NEW because if the INSERT failed, it will have thrown an exception anyway.
  -- TODO: test this!

  -- If we don't set this, a RETURNING clause on the INSERT won't
  -- return the generated ID!
  NEW.id = new_user_id;
  RETURN NEW;
END;
$$;

COMMIT;

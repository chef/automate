-- Deploy update_token_birthday
-- requires: users

BEGIN;

CREATE OR REPLACE FUNCTION update_token_birthday()
RETURNS TRIGGER
LANGUAGE plpgsql
AS $$
BEGIN
  IF TG_OP = 'UPDATE' AND NEW.auth_token_bday != OLD.auth_token_bday THEN
    -- Naughty, naughty... only *I* get to change that field!
    NEW.auth_token_bday = OLD.auth_token_bday;
  END IF;

  -- Only update the birthday if it's really a new token
  IF (TG_OP = 'UPDATE' AND NEW.auth_token != OLD.auth_token) OR
     TG_OP = 'INSERT' THEN
    -- This is the actual real time, not the time at the start of the
    -- transaction, so multiple tokens updated in the same transaction
    -- will all have slightly different birthdays.
    --
    -- But really, how often is THAT going to happen, though?  Being
    -- different by a few milliseconds isn't going to kill anything,
    -- and it's easier to test.
    NEW.auth_token_bday = clock_timestamp();
  END IF;
  RETURN NEW;
END;
$$;

COMMIT;

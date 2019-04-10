CREATE TYPE migration_state
  AS ENUM (
    'init', -- initial state
    'in-progress',
    'successful',
    'failed');

CREATE FUNCTION valid_migration_state_change(
  old_state migration_state, new_state migration_state
) RETURNS boolean
LANGUAGE sql AS
$$
  SELECT EXISTS(
    SELECT 1
    FROM (VALUES
      ('init',        'in-progress'),
      ('in-progress', 'failed'),
      ('in-progress', 'successful'),
      ('in-progress', 'failed'),
      ('failed',      'in-progress'),
      ('failed',      'init'),
      ('successful',  'init')
    ) AS valid(old, new)
    WHERE valid.old::migration_state=old_state
    AND valid.new::migration_state=new_state
  )
$$;

CREATE OR REPLACE FUNCTION migration_trigger_func() RETURNS trigger
LANGUAGE plpgsql AS $$
DECLARE
BEGIN
  IF OLD IS NULL THEN
    RETURN NEW;
  END IF;
  IF valid_migration_state_change(OLD.state, NEW.state) THEN
    RETURN NEW;
  END IF;
  RAISE EXCEPTION 'invalid state change: % -> %', OLD.state, NEW.state;
END
$$;

CREATE TABLE migration_status (
  only_one_row boolean PRIMARY KEY DEFAULT TRUE,
  state migration_state NOT NULL,
  CONSTRAINT only_one_row_unique CHECK (only_one_row)
);

CREATE TRIGGER migration_trigger_func BEFORE UPDATE ON migration_status
  FOR EACH ROW EXECUTE PROCEDURE migration_trigger_func();

-- init state
INSERT INTO migration_status (state) VALUES ('init'::migration_state);

BEGIN;

CREATE TYPE staged_rule_state
  AS ENUM (
    'new',
    'deleted',
    'updated'
    );

CREATE TABLE iam_staged_project_rules (
  db_id SERIAL PRIMARY KEY,
  id TEXT NOT NULL UNIQUE,
  project_id TEXT REFERENCES iam_projects,
  name TEXT NOT NULL,
  type TEXT NOT NULL,
  updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  state staged_rule_state NOT NULL
);

CREATE TABLE iam_staged_rule_conditions (
  db_id SERIAL PRIMARY KEY,
  rule_db_id INTEGER REFERENCES iam_staged_project_rules ON DELETE CASCADE,
  value TEXT[] NOT NULL,
  attribute TEXT NOT NULL,
  operator TEXT NOT NULL,
  updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE FUNCTION valid_staged_rule_state_change(
  old_state staged_rule_state, new_state staged_rule_state
) RETURNS boolean
LANGUAGE sql AS
$$
  SELECT EXISTS(
    SELECT 1
    FROM (VALUES
      ('updated', 'deleted'),
      ('updated', 'updated')
    ) AS valid(old, new)
    WHERE valid.old::staged_rule_state=old_state
    AND valid.new::staged_rule_state=new_state
  )
$$;

CREATE OR REPLACE FUNCTION staged_rule_state_trigger_func() RETURNS trigger
LANGUAGE plpgsql AS $$
DECLARE
BEGIN
  IF OLD IS NULL THEN
    RETURN NEW;
  END IF;
  IF valid_staged_rule_state_change(OLD.state, NEW.state) THEN
    RETURN NEW;
  END IF;
  RAISE EXCEPTION 'invalid state change: % -> %', OLD.state, NEW.state;
END
$$;

CREATE TRIGGER staged_rule_state_trigger_func BEFORE UPDATE ON iam_staged_project_rules
  FOR EACH ROW EXECUTE PROCEDURE staged_rule_state_trigger_func();

COMMIT;
BEGIN;

CREATE TYPE force_upgrade_state
  AS ENUM (
    'init',
    'successful',
    'failed');

CREATE TABLE force_upgrade_status (
  only_one_row boolean PRIMARY KEY DEFAULT TRUE,
  state force_upgrade_state NOT NULL,
  CONSTRAINT only_one_row_unique CHECK (only_one_row)
);

INSERT INTO force_upgrade_status (state) VALUES ('init'::force_upgrade_state);

COMMIT;

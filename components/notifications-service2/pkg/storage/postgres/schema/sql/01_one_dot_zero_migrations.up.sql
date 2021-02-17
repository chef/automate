-- 1.0 MIGRATIONS:
-- The migrations in this file are imported from the previous elixir
-- implementation of the service. Here they are modified to use idempotent
-- forms which are expected to be safe to run on a database that already has
-- them. After running these statements, we expect databases for both upgrade
-- and fresh install cases to have identical schemas.
--
-- If you need to refer to the original migration code, see:
-- https://github.com/chef/automate/blob/a053722bb4f974f28d4e7159841d34e4da27a5de/components/notifications-service/server/lib/data/migrations.ex

CREATE TABLE IF NOT EXISTS migrations (num INTEGER NOT NULL, descr TEXT, at TIMESTAMPTZ NOT NULL);

DO $$
BEGIN
    IF NOT EXISTS (SELECT 1 FROM pg_type WHERE typname = 'rule_event') THEN
        CREATE TYPE rule_event AS ENUM ('CCRSuccess',
                                        'CCRFailure',
                                        'ComplianceSuccess',
                                        'ComplianceFailure',
                                        'Assets');
    END IF;
END
$$;

DO $$
BEGIN
    IF NOT EXISTS (SELECT 1 FROM pg_type WHERE typname = 'rule_action') THEN
        CREATE TYPE rule_action AS ENUM ('SlackAlert', 'WebhookAlert', 'ServiceNowAlert');
    END IF;
END
$$;

CREATE TABLE IF NOT EXISTS rules (
     id uuid PRIMARY KEY,
   name TEXT UNIQUE NOT NULL,
  event rule_event NOT NULL,
 action rule_action NOT NULL,
    url TEXT NOT NULL );

CREATE UNLOGGED TABLE IF NOT EXISTS processed_events (
    inbound_id VARCHAR(64),
         event rule_event,
            at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
   PRIMARY KEY (inbound_id, event)
);

CREATE OR REPLACE FUNCTION log_and_clean_event(
                   id varchar(64),
           event_type rule_event,
    delete_older_than SMALLINT -- purge records older than this # of seconds
  )
  RETURNS BOOLEAN -- True if the event exists
  LANGUAGE plpgsql
AS $$
  DECLARE
    already_processed BOOLEAN;
BEGIN
    already_processed = false;
    -- First clean up old events:
    DELETE FROM processed_events
     WHERE at < (CURRENT_TIMESTAMP - (delete_older_than * interval '1 second'));
    -- Now try to insert - failure due to duplicate insert means we already
    -- processed the event.
    BEGIN
      INSERT INTO processed_events(inbound_id, event, at)
           VALUES (id, event_type, CURRENT_TIMESTAMP);
    EXCEPTION WHEN unique_violation THEN
      already_processed = true;
    END;
    RETURN already_processed;
  END;
  $$;

ALTER TABLE rules ADD COLUMN IF NOT EXISTS secret_id VARCHAR;


ALTER TABLE rules ADD COLUMN IF NOT EXISTS critical_controls_only BOOLEAN;
ALTER TABLE rules ALTER COLUMN critical_controls_only SET DEFAULT FALSE;

UPDATE rules SET critical_controls_only=FALSE;
DELETE from rules where event = 'Assets';

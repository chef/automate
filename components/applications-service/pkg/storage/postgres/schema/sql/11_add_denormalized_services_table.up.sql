CREATE TABLE IF NOT EXISTS service_full (
  id                        SERIAL    PRIMARY KEY,

  -- package identity. service "name" is the package name
  origin                    TEXT      NOT NULL,
  name                      TEXT      NOT NULL,
  version                   TEXT      NOT NULL,
  release                   TEXT      NOT NULL,
  package_ident             TEXT      NOT NULL,

  -- health check data
  health                    TEXT      NOT NULL,
  health_check_message      TEXT      NOT NULL,

  -- updates
  channel                   TEXT      NOT NULL,
  update_strategy           TEXT      NOT NULL,

  -- supervisor-specific data
  supervisor_id             TEXT      NOT NULL,
  fqdn                      TEXT      NOT NULL DEFAULT '',
  site                      TEXT      NOT NULL DEFAULT '',

  service_group_name        TEXT      NOT NULL,
  service_group_name_suffix TEXT      NOT NULL,

  -- deployment data
  application               TEXT      NOT NULL DEFAULT '',
  environment               TEXT      NOT NULL DEFAULT '',

  -- "time wizard" -- what the previous status was, and when it changed
  previous_health           TEXT      NOT NULL DEFAULT 'NONE',
  health_updated_at         TIMESTAMP NOT NULL DEFAULT NOW(),

  -- time of last health check, used to determine "disconnected" services
  last_event_occurred_at    TIMESTAMP NOT NULL DEFAULT NOW(),

  -- informational, not used in the application
  created_at                TIMESTAMP NOT NULL DEFAULT NOW(),
  updated_at                TIMESTAMP NOT NULL DEFAULT NOW(),

  -- A supervisor can have only one of a service with a given name;
  -- origin, release, etc. can change.
  UNIQUE (name, supervisor_id)
);

-- Create indexes for search-able things
-- TODO/question for review: we might want to use this technique to do
-- case-insensitive LIKE queries with an index:
-- https://www.postgresql.org/docs/9.6/indexes-expressional.html
-- but if we do that, then we also need a not-lower one to do any case-sensitive stuff?
CREATE INDEX service_full_name_idx ON service_full (name);
CREATE INDEX service_full_origin_idx ON service_full (origin);
CREATE INDEX service_full_service_group_name_suffix_idx ON service_full (service_group_name_suffix);
CREATE INDEX service_full_version_idx ON service_full (version);
CREATE INDEX service_full_release_idx ON service_full (release);
CREATE INDEX service_full_environment_idx ON service_full (environment);
CREATE INDEX service_full_application_idx ON service_full (application);
CREATE INDEX service_full_channel_idx ON service_full (channel);
CREATE INDEX service_full_site_idx ON service_full (site);


CREATE TRIGGER update_service_full_updated_at BEFORE UPDATE
ON service_full FOR EACH ROW EXECUTE PROCEDURE
update_timestamp_updated_at_column();

-- Implement the "time wizard" -- when an update changes `health` then copy
-- health from old row to new row's `previous_health` column.
CREATE OR REPLACE FUNCTION save_previous_health_on_state_change()
RETURNS TRIGGER
LANGUAGE plpgsql
AS $$
BEGIN
  IF NEW.health != OLD.health THEN
    NEW.previous_health := OLD.health;
    NEW.health_updated_at := NOW();
    RETURN NEW;
  END IF;
  RETURN NEW;
END;
$$;

CREATE TRIGGER service_full_save_previous_health_on_state_change BEFORE UPDATE
ON service_full FOR EACH ROW EXECUTE PROCEDURE
save_previous_health_on_state_change();


-- TODO: remove this, just for testing.
DROP TABLE service_full;
CREATE TABLE IF NOT EXISTS service_full (
  id                        SERIAL    PRIMARY KEY,

  -- package identity. service "name" is the package name
  origin                    TEXT      NOT NULL,
  name                      TEXT      NOT NULL,
  version                   TEXT      NOT NULL,
  release                   TEXT      NOT NULL,
  package_ident             TEXT      NOT NULL,

  -- health check data
  status                    TEXT      NOT NULL,
  health                    TEXT      NOT NULL,

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
  -- TODO: figure out how to use a trigger to fill this out when updating (?)
  previous_health           TEXT      NOT NULL DEFAULT 'NONE',
  health_updated_at         TIMESTAMP NOT NULL DEFAULT NOW(),

  -- time of last health check, used to determine "disconnected" services
  last_event_occurred_at    TIMESTAMP NOT NULL DEFAULT NOW(),

  -- informational, not used in the application
  -- TODO: triggers
  created_at                TIMESTAMP NOT NULL DEFAULT NOW(),
  updated_at                TIMESTAMP NOT NULL DEFAULT NOW(),

  -- TODO: is this correct?
  UNIQUE (name, supervisor_id)
);


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


-- service       | id                     | integer
-- service       | origin                 | text
-- service       | name                   | text
-- service       | version                | text
-- service       | release                | text
-- service       | status                 | text
-- service       | health                 | text
-- service       | channel                | text
-- service       | update_strategy        | text

-- service       | package_ident          | text
-- service       | last_event_occurred_at | timestamp without time zone
--  service       | created_at             | timestamp without time zone
--  service       | updated_at             | timestamp without time zone
-- service       | previous_health        | text
-- service       | health_updated_at      | timestamp without time zone
-- supervisor    | member_id              | text
-- supervisor    | fqdn                   | text
-- supervisor    | site                   | text
-- service_group | name                   | text
-- service_group | name_suffix            | text
-- deployment    | app_name               | text
-- deployment    | environment            | text


-- SELECT table_name, column_name, data_type
--   FROM information_schema.columns
--  WHERE table_schema = 'public'
--    AND table_name IN ('deployment', 'service', 'service_group', 'supervisor')
--      ;


-- INSERT INTO service_full (
--   origin, name, version, release, package_ident, status, health,
--   channel, update_strategy, supervisor_id, fqdn, site,
--   service_group_name, service_group_name_suffix,
--   application, environment
-- )
-- VALUES (
--   'core', 'pkg', '0.1.0', '2019blahblah', 'core/pkg/0.1.0/2019blahblah', '', 'CRITICAL',
--   'stable', 'AT-ONCE', '1111', 'example.com', 'default',
--   'pkg.default', 'default',
--   'myapp', 'myenv'
-- )
-- ON CONFLICT ON CONSTRAINT service_full_name_supervisor_id_key
-- DO UPDATE SET (
--   origin, version, release, package_ident, status, health,
--   channel, update_strategy, fqdn, site,
--   service_group_name, service_group_name_suffix,
--   application, environment
-- ) = (
--   EXCLUDED.origin, EXCLUDED.version, EXCLUDED.release, EXCLUDED.package_ident, EXCLUDED.status, EXCLUDED.health,
--   EXCLUDED.channel, EXCLUDED.update_strategy, EXCLUDED.fqdn, EXCLUDED.site,
--   EXCLUDED.service_group_name, EXCLUDED.service_group_name_suffix,
--   EXCLUDED.application, EXCLUDED.environment
-- )
-- ;


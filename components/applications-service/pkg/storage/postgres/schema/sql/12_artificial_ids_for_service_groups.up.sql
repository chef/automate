ALTER TABLE service_full ADD COLUMN service_group_id text;
CREATE INDEX service_full_service_group_id ON service_full (service_group_id);

--------------------------------------------------------------------------------
-- Data Migration
--------------------------------------------------------------------------------
-- Move data from the normalized tables to the denormalized one.
--
-- The columns in service_full are:
--
--   table_name  |        column_name        |          data_type
-- --------------+---------------------------+-----------------------------
--  service_full | id                        | integer
--  service_full | origin                    | text
--  service_full | name                      | text
--  service_full | version                   | text
--  service_full | release                   | text
--  service_full | package_ident             | text
--  service_full | health                    | text
--  service_full | health_check_message      | text
--  service_full | channel                   | text
--  service_full | update_strategy           | text
--  service_full | supervisor_id             | text
--  service_full | fqdn                      | text
--  service_full | site                      | text
--  service_full | service_group_name        | text
--  service_full | service_group_name_suffix | text
--  service_full | application               | text
--  service_full | environment               | text
--  service_full | previous_health           | text
--  service_full | health_updated_at         | timestamp without time zone
--  service_full | last_event_occurred_at    | timestamp without time zone
--  service_full | created_at                | timestamp without time zone
--  service_full | updated_at                | timestamp without time zone
--  service_full | service_group_id          | text
--
-- We need to NOT import the id or else we have to manually reset the sequence
-- that generates the id numbers; everything else we want to keep.
INSERT INTO service_full(
  origin,
  name,
  version,
  release,
  package_ident,
  health,
  health_check_message,
  channel,
  update_strategy,
  supervisor_id,
  fqdn,
  site,
  service_group_name,
  service_group_name_suffix,
  application,
  environment,
  previous_health,
  health_updated_at,
  last_event_occurred_at,
  created_at,
  updated_at,
  service_group_id
  ) ( SELECT
    s.origin AS origin
  , s.name AS name
  , s.version AS version
  , s.release AS release
  , s.package_ident AS package_ident
  , s.health AS health
  , s.status AS health_check_message
  , s.channel AS channel
  , s.update_strategy AS update_strategy
  , sup.member_id AS supervisor_id
  , sup.fqdn AS fqdn
  , sup.site AS site
  , sg.name AS service_group_name
  , sg.name_suffix AS service_group_name_suffix
  , d.app_name AS application
  , d.environment AS environment
  , s.previous_health as previous_health
  , s.health_updated_at as health_updated_at
  , s.last_event_occurred_at as last_event_occurred_at
  , s.created_at AS created_at
  , s.updated_at AS updated_at
	-- serviceGroupUniques := fmt.Sprintf("%s:%s:%s", svcMetadata.GetServiceGroup(), eventMetadata.GetApplication(), eventMetadata.GetEnvironment())
  , encode(digest(CONCAT(sg.name, ':', d.app_name, ':', d.environment), 'sha256'), 'hex') as service_group_id
FROM service AS s
LEFT JOIN service_group AS sg
  ON s.group_id = sg.id
LEFT JOIN deployment AS d
  ON s.deployment_id = d.id
LEFT JOIN supervisor AS sup
  ON s.sup_id = sup.id );

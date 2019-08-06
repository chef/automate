-- Add service group name suffix column, for easier search by service group name
-- The suffix is everything after the '.', (eg: for nginx.default the suffix is default)
ALTER TABLE service_group ADD COLUMN name_suffix TEXT NOT NULL DEFAULT '';

UPDATE service_group
SET name_suffix = substring(name FROM '\.(.*)');

-- View won't accept new columns, so we must kill the view.
DROP VIEW service_group_health;

--Add service group name suffix to view so we can search by it.
CREATE OR REPLACE VIEW service_group_health AS
SELECT *
       ,(CASE WHEN health_critical > 0 THEN '1_CRITICAL'
              WHEN health_unknown  > 0 THEN '2_UNKNOWN'
              WHEN health_warning  > 0 THEN '3_WARNING'
              ELSE '4_OK' END ) as health
  FROM (SELECT  sg.id
               ,sg.deployment_id
               ,sg.name as name
               ,COUNT(s.health) FILTER (WHERE s.health = 'OK') AS health_ok
               ,COUNT(s.health) FILTER (WHERE s.health = 'CRITICAL') AS health_critical
               ,COUNT(s.health) FILTER (WHERE s.health = 'WARNING') AS health_warning
               ,COUNT(s.health) FILTER (WHERE s.health = 'UNKNOWN') AS health_unknown
               ,COUNT(s.health) AS health_total
               ,round((COUNT(s.health) FILTER (WHERE s.health = 'OK')
                     / COUNT(s.health)::float) * 100) as percent_ok
               ,(SELECT array_agg(DISTINCT CONCAT (s.package_ident))
                   FROM service AS s
                  WHERE s.group_id = sg.id) AS releases
               ,d.app_name as app_name
               ,d.environment as environment
               ,sg.name_suffix as name_suffix
        FROM service_group AS sg
        JOIN service AS s
             ON s.group_id = sg.id
        JOIN deployment as d
             ON sg.deployment_id = d.id
       GROUP BY sg.id, sg.deployment_id, sg.name, d.app_name, d.environment, sg.name_suffix
       ) AS service_groups_health_calculation;

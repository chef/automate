-- Deploy delivery:upsert_notification_config to pg
-- requires: notification_config, notification_config_constraints

-- This procedure could be replaced with a single UPSERT call when it
-- rolls around in PGSQL
-- (see http://www.craigkerstiens.com/2015/05/08/upsert-lands-in-postgres-9.5/)

BEGIN;

-- We have to drop the previous function because it has a different # of parameters
-- If we do not drop this we will end up with two different functions with the same name.
DROP FUNCTION IF EXISTS upsert_notification_config(
    p_notification_type notification_config.notification_type%TYPE,
    p_name notification_config.name%TYPE,
    p_settings notification_config.settings%TYPE,
    p_enabled notification_config.enabled%TYPE,
    p_organization_id organizations.id%TYPE,
    p_enterprise_id enterprises.id%TYPE,
    p_project_id projects.id%TYPE
);

CREATE OR REPLACE FUNCTION upsert_notification_config(
    p_notification_type notification_config.notification_type%TYPE,
    p_name notification_config.name%TYPE,
    p_settings notification_config.settings%TYPE,
    p_enabled notification_config.enabled%TYPE,
    p_organization_id notification_config.organization_id%TYPE,
    p_enterprise_id notification_config.enterprise_id%TYPE
)
RETURNS SETOF notification_config
ROWS 1
LANGUAGE plpgsql
AS $$
BEGIN
  -- try to insert
    RETURN QUERY
    INSERT INTO notification_config(
         notification_type,
         name,
         settings,
         enabled,
         organization_id,
         enterprise_id)
      VALUES (p_notification_type,
              p_name,
              p_settings,
              p_enabled,
              p_organization_id,
              p_enterprise_id)
      RETURNING notification_config.*;
    EXCEPTION WHEN unique_violation THEN

    RETURN QUERY
    UPDATE notification_config
    SET    notification_type = p_notification_type,
           name = p_name,
           settings = p_settings,
           enabled = p_enabled
    WHERE  (
            organization_id = p_organization_id
            AND organization_id IS NOT NULL
           )
       OR
           (
            enterprise_id = p_enterprise_id
            AND enterprise_id IS NOT NULL
           )
       AND  notification_type = p_notification_type
    RETURNING notification_config.*;

END;
$$;

COMMIT;

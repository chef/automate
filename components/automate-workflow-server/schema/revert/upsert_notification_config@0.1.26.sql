-- Revert delivery:upsert_notification_config to pg
-- requires: notification_config, notification_config_constraints

-- This procedure could be replaced with a single UPSERT call when it
-- rolls around in PGSQL. This will be available in 9.5

BEGIN;

-- We have to drop this function because it cannot be replaced due to the
-- different number of parameters
DROP FUNCTION IF EXISTS upsert_notification_config(
    p_notification_type notification_config.notification_type%TYPE,
    p_name notification_config.name%TYPE,
    p_settings notification_config.settings%TYPE,
    p_enabled notification_config.enabled%TYPE,
    p_organization_id notification_config.organization_id%TYPE,
    p_enterprise_id notification_config.enterprise_id%TYPE
);

--Replace function with previous incarnation (and 1 less parameter)
CREATE OR REPLACE FUNCTION upsert_notification_config(
    p_notification_type notification_config.notification_type%TYPE,
    p_name notification_config.name%TYPE,
    p_settings notification_config.settings%TYPE,
    p_enabled notification_config.enabled%TYPE,
    p_organization_id notification_config.organization_id%TYPE
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
         organization_id)
      VALUES (p_notification_type,
              p_name,
              p_settings,
              p_enabled,
              p_organization_id)
      RETURNING notification_config.*;
    EXCEPTION WHEN unique_violation THEN

    RETURN QUERY
    UPDATE notification_config
    SET    notification_type = p_notification_type,
           name = p_name,
           settings = p_settings,
           enabled = p_enabled
    WHERE  organization_id = p_organization_id
    RETURNING notification_config.*;

END;
$$;

COMMIT;

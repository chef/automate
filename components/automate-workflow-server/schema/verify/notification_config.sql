-- Verify delivery:notification_config on pg

BEGIN;

SELECT  id,
        notification_type,
        name,
        settings,
        enabled,
        organization_id,
        project_id
   FROM notification_config WHERE FALSE;

ROLLBACK;
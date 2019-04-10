-- Deploy delivery:insert_notification_subscription to pg
-- requires: notification_subscriptions

BEGIN;

  CREATE OR REPLACE FUNCTION upsert_notification_subscription(
    p_enterprise_name enterprises.name%TYPE,
    p_organization_name organizations.name%TYPE,
    p_project_name projects.name%TYPE,
    p_user_name users.name%TYPE,
    p_categories TEXT[]
  )
  RETURNS SETOF notification_subscriptions
  ROWS 1
  LANGUAGE plpgsql
  AS $$
    DECLARE
      v_project_id projects.id%TYPE;
      v_user_id users.id%TYPE;
  BEGIN

    SELECT p.id
      FROM projects p, organizations o, enterprises e
     WHERE p.name = p_project_name
       AND p.organization_id = o.id
       AND o.name = p_organization_name
       AND o.enterprise_id = e.id
       AND e.name = p_enterprise_name
      INTO v_project_id;

    SELECT u.id
      FROM users u, enterprises e
     WHERE u.name = p_user_name
       AND u.enterprise_id = e.id
       AND e.name = p_enterprise_name
      INTO v_user_id;

    DELETE FROM notification_subscriptions n
     WHERE n.project_id = v_project_id
       AND n.user_id = v_user_id;

    -- If there are no categories then we remove the subscription, otherwise insert
    IF array_length(p_categories, 1) <> 0 THEN

     RETURN QUERY
      INSERT INTO notification_subscriptions (project_id, user_id, categories)
           VALUES (v_project_id, v_user_id, p_categories)
        RETURNING notification_subscriptions.*;

    END IF;

  END;
  $$;

COMMIT;

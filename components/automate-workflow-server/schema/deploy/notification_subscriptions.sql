-- Deploy delivery:notification_subscriptions to pg

BEGIN;

  CREATE TABLE IF NOT EXISTS notification_subscriptions (
    id BIGSERIAL PRIMARY KEY,
    project_id BIGINT NOT NULL REFERENCES projects(id) ON UPDATE CASCADE ON DELETE CASCADE,
    user_id BIGINT NOT NULL REFERENCES users(id) ON UPDATE CASCADE ON DELETE CASCADE
  );

  CREATE UNIQUE INDEX notification_subscriptions_project_id_user_id_key
                   ON notification_subscriptions (project_id, user_id);

COMMIT;

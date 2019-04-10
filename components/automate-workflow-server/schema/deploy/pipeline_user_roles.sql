-- Deploy pipeline_user_roles
-- requires: pipelines
-- requires: delivery_role

BEGIN;

CREATE TABLE IF NOT EXISTS pipeline_user_roles(
  pipeline_id BIGINT NOT NULL REFERENCES pipelines(id) ON UPDATE CASCADE ON DELETE CASCADE,
  user_id     BIGINT NOT NULL REFERENCES users(id) ON UPDATE CASCADE ON DELETE CASCADE,
  role delivery_role NOT NULL,
  PRIMARY KEY(pipeline_id, user_id, role)
);

COMMIT;

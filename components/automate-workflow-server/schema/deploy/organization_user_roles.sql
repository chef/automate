-- Deploy organization_user_roles

BEGIN;

CREATE TABLE IF NOT EXISTS organization_user_roles(
  organization_id BIGINT        NOT NULL REFERENCES organizations(id) ON UPDATE CASCADE ON DELETE CASCADE,
  user_id         BIGINT        NOT NULL REFERENCES users(id)  ON UPDATE CASCADE ON DELETE CASCADE,
  role            delivery_role NOT NULL,

  PRIMARY KEY(organization_id, user_id, role)
);

COMMIT;

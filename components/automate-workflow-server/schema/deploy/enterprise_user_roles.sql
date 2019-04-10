-- Deploy enterprise_user_roles

BEGIN;

CREATE TABLE IF NOT EXISTS enterprise_user_roles(
  enterprise_id BIGINT NOT NULL REFERENCES enterprises(id)  ON UPDATE CASCADE ON DELETE CASCADE,
  user_id       BIGINT NOT NULL REFERENCES users(id) ON UPDATE CASCADE ON DELETE CASCADE,
  -- NOTE: we could just have one FK referencing users(id,
  -- enterprise_id) if there was actually a unique constraint on that;
  -- alternatively, we could have 'user_id' be 'user_name', in which
  -- case we *could* have the single constraint, at the expense of
  -- this table being different from all the others :/
  role delivery_role NOT NULL,
  PRIMARY KEY(enterprise_id, user_id, role)
);

COMMIT;

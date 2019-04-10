-- Deploy organizations
-- requires: enterprises

BEGIN;

CREATE TABLE organizations (
  id BIGSERIAL PRIMARY KEY,
  enterprise_id BIGINT NOT NULL REFERENCES enterprises(id) ON DELETE CASCADE,
  name TEXT NOT NULL,
  UNIQUE(enterprise_id, name)
);

COMMENT ON TABLE organizations IS
$$Each organization is scoped to an enterprise.
$$;

COMMIT;

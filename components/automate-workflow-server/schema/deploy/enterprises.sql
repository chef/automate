-- Deploy enterprises

BEGIN;

CREATE TABLE enterprises (
  id BIGSERIAL PRIMARY KEY,
  name TEXT NOT NULL UNIQUE
);

COMMENT ON TABLE enterprises IS
$$An enterprises is the top level concept
in a Chef Delivery install. Users and orgs
are scoped to an enterprise.
$$;

COMMIT;

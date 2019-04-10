-- Deploy users
-- requires: enterprises

BEGIN;


CREATE TABLE users(
  id BIGSERIAL PRIMARY KEY,
  enterprise_id BIGINT NOT NULL REFERENCES enterprises(id) ON UPDATE CASCADE ON DELETE CASCADE,
  name TEXT NOT NULL, -- these can be anything: foo@bigco.com, //PLACE/STUFF/BOB, etcXS
  UNIQUE(enterprise_id, name),
  ssh_pub_key ssh_pub_key,
  first_name TEXT,
  last_name TEXT,
  email TEXT, -- TODO: This should probably be NOT NULL, right?
  -- TODO: shouldn't we have an index on user_type since
  -- we filter on that to generate the internal_users view?
  user_type user_type NOT NULL
);

COMMENT ON TABLE users IS
'Common fields for all users, whether they are internally defined or
linked in from external sources (e.g., LDAP)';

COMMIT;

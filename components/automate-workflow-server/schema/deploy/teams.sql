-- Deploy users
-- requires: enterprises

BEGIN;


CREATE TABLE teams(
  id BIGSERIAL PRIMARY KEY,
  enterprise_id BIGINT NOT NULL REFERENCES enterprises(id) ON UPDATE CASCADE ON DELETE CASCADE,
  name TEXT NOT NULL, -- these can be anything: foo@bigco.com, //PLACE/STUFF/BOB, etcXS
  UNIQUE(enterprise_id, name),
  description TEXT
);

COMMENT ON TABLE teams IS
'Name and description for teams';

COMMIT;

-- Deploy projects
-- requires: organizations

BEGIN;

CREATE TABLE IF NOT EXISTS projects(
  id BIGSERIAL PRIMARY KEY,
  organization_id BIGINT NOT NULL REFERENCES organizations(id) ON UPDATE CASCADE ON DELETE CASCADE,
  -- The GUID field below is used to know where to look for the project on the disk
  -- It's better to use something random for that (as opposed to e.g. just take
  -- the numerical ID) to avoid having a predictable naming scheme for repos
  -- on the disk - better security
  -- That field is automatically generated on inserts
  guid UUID NOT NULL,
  UNIQUE(guid),
  name TEXT NOT NULL,
  UNIQUE(organization_id, name)
);

COMMIT;

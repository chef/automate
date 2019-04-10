-- Deploy delivery:change_bitbucket_metadata to pg
-- requires: changes

BEGIN;

CREATE TABLE IF NOT EXISTS change_bitbucket_metadata(
  id UUID NOT NULL PRIMARY KEY DEFAULT uuid_generate_v4() ,
  change_id UUID NOT NULL UNIQUE REFERENCES changes(id) ON DELETE CASCADE,
  pr_id BIGINT NOT NULL,
  pr_url TEXT NOT NULL,
  UNIQUE(pr_id, pr_url)
);

COMMIT;

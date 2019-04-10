-- Deploy patchsets
-- requires: changes, users

BEGIN;

CREATE TABLE IF NOT EXISTS patchsets(
  id BIGSERIAL PRIMARY KEY,
  change_id UUID NOT NULL REFERENCES changes(id) ON UPDATE CASCADE ON DELETE CASCADE,
  sequence_number SMALLINT NOT NULL,
  submitted_at cd_timestamp NOT NULL,
  UNIQUE(change_id, sequence_number),
  sha TEXT NOT NULL,
  -- TODO: we need to have this notion of a former user here,
  -- as we don't want to delete patchsets submitted by former users
  submitter_id BIGINT NOT NULL REFERENCES users(id) ON DELETE CASCADE,
  -- the latest sha against which this patch set was verified
  verified_against_sha TEXT,
  is_verified boolean NOT NULL DEFAULT FALSE,
  status cd_patchset_status NOT NULL
);

COMMIT;

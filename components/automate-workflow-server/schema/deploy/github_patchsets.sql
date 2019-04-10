-- Deploy delivery:github_patchsets to pg
-- requires: patchsets

BEGIN;

CREATE TABLE IF NOT EXISTS github_patchsets(
  id BIGSERIAL PRIMARY KEY,
  patchset_id BIGSERIAL NOT NULL REFERENCES patchsets(id) ON UPDATE CASCADE ON DELETE CASCADE,
  payload JSON NOT NULL
);


COMMIT;

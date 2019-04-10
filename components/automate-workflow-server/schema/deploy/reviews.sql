-- Deploy reviews
-- requires: patchsets, users

BEGIN;

CREATE TABLE IF NOT EXISTS reviews(
  id BIGSERIAL PRIMARY KEY,
  patchset_id BIGINT NOT NULL REFERENCES patchsets(id) ON DELETE CASCADE,
  -- TODO: here too, we want this notion of 'former users'
  user_id BIGINT NOT NULL REFERENCES users(id) ON DELETE CASCADE,
  UNIQUE(patchset_id, user_id),
  -- TODO: wasn't in the spec, but we've been mentioning quite a few times
  -- in team meetings that we'd like 'real-arithmetic' review scores
  numeric_value SMALLINT NOT NULL
);

COMMIT;

-- Deploy user_cache

BEGIN;

CREATE TABLE user_cache(
  id BIGSERIAL PRIMARY KEY NOT NULL REFERENCES users(id) ON UPDATE CASCADE ON DELETE CASCADE,
  cached_at cd_timestamp NOT NULL
);

COMMENT ON TABLE user_cache IS
'The information for externally-defined users is periodically cached
in the database; this table indicates when a user was last cached. It
is automatically populated and maintained.';

COMMIT;

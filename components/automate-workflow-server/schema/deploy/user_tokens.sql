-- Deploy user_tokens

BEGIN;

CREATE TABLE user_tokens(
  id BIGSERIAL NOT NULL REFERENCES users(id) ON UPDATE CASCADE ON DELETE CASCADE,
  auth_token TEXT NOT NULL,
  PRIMARY KEY(id, auth_token),
  auth_token_bday cd_timestamp NOT NULL
);

COMMENT ON TABLE user_tokens IS

'All users, whether they are internal or external, will have
zero-or-more tokens for authenticating to the system. Lack of a row in
this table for a given user indicates that a new token must be
generated.';

COMMIT;

-- TODO: Disallow UPDATE on this table?

-- Deploy delivery:password_reset_tokens to pg

BEGIN;

CREATE TABLE password_reset_tokens(
  user_id BIGSERIAL NOT NULL REFERENCES users(id) ON UPDATE CASCADE ON DELETE CASCADE,
  token TEXT NOT NULL,
  PRIMARY KEY(user_id, token),
  expiry cd_timestamp NOT NULL
);

COMMENT ON TABLE password_reset_tokens IS

'Tokens to be used for password reset.';

COMMIT;

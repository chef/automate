-- Deploy user_passwords

BEGIN;

CREATE TABLE user_passwords(
  id BIGSERIAL PRIMARY KEY NOT NULL REFERENCES users(id) ON UPDATE CASCADE ON DELETE CASCADE,
  hashed_pass TEXT NOT NULL,
  hash_type password_hash_type NOT NULL
);

COMMENT ON TABLE user_passwords IS
'Internal users (and only internal users) have password information
stored in the database. Lack of a row in this table for a given user
indicates that a password needs to be reset.';

COMMIT;

-- Deploy delivery:oauth_user_aliases_id to pg
-- requires: oauth_user_aliases

BEGIN;

ALTER TABLE IF EXISTS oauth_user_aliases ADD COLUMN id BIGSERIAL PRIMARY KEY;

COMMIT;

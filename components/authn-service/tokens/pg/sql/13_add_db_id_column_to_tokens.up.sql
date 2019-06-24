BEGIN;

ALTER TABLE chef_authn_tokens DROP CONSTRAINT chef_authn_clients_pkey;
ALTER TABLE chef_authn_tokens ADD CONSTRAINT chef_authn_tokens_id_unique UNIQUE (id);
ALTER TABLE chef_authn_tokens ADD COLUMN db_id SERIAL PRIMARY KEY;

COMMIT;

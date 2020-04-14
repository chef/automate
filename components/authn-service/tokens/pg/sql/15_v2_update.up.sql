BEGIN;

ALTER TABLE chef_authn_tokens RENAME COLUMN description TO name;
COMMIT;
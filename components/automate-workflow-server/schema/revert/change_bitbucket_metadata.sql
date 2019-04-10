-- Revert delivery:change_bitbucket_metadata from pg

BEGIN;

DROP TABLE IF EXISTS change_bitbucket_metadata;

COMMIT;

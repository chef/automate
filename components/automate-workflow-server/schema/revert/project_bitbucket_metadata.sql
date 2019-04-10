-- Revert delivery:project_bitbucket_metadata from pg

BEGIN;

DROP TABLE IF EXISTS project_bitbucket_metadata;

COMMIT;

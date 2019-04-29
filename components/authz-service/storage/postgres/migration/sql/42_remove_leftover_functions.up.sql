BEGIN;

-- These are leftover, deprecated functions, whose signatures have changed.

DROP FUNCTION IF EXISTS query_projects();

DROP FUNCTION IF EXISTS query_project(TEXT);

DROP FUNCTION IF EXISTS query_policies();

DROP FUNCTION IF EXISTS query_policy(TEXT);

COMMIT;

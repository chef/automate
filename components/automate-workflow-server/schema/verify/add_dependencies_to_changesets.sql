-- Verify delivery:add_dependencies_to_changesets on pg

BEGIN;

SELECT dependencies
FROM changesets
WHERE false;

ROLLBACK;

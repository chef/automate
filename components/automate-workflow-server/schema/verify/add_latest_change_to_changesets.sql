-- Verify delivery:add_latest_change_to_changesets on pg

BEGIN;

SELECT latest_change_id
FROM changesets
WHERE false;

ROLLBACK;

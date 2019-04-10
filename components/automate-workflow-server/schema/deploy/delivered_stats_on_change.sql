-- Deploy delivered_stats_on_change

BEGIN;

ALTER TABLE changes ADD COLUMN delivered_at cd_timestamp DEFAULT NULL;
ALTER TABLE changes ADD COLUMN delivered_by TEXT;

UPDATE changes
SET delivered_at = changesets.delivered_at,
    delivered_by = changesets.delivered_by
FROM changesets
WHERE changes.changeset_id = changesets.id
AND changesets.status = 'closed';

COMMIT;

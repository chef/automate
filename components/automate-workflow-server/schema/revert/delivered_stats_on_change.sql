-- Revert delivered_stats_on_change

BEGIN;

ALTER TABLE changes DROP COLUMN delivered_at;
ALTER TABLE changes DROP COLUMN delivered_by;

COMMIT;

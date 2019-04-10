-- Revert delivery:scanner_add_node_managers from pg

BEGIN;

DROP TABLE IF EXISTS node_managers;

ALTER TABLE jobs
  DROP COLUMN IF EXISTS node_selectors;

COMMIT;
-- Revert delivery:scanner_add_nodes_last_scan from pg

BEGIN;

ALTER TABLE nodes
  DROP COLUMN IF EXISTS last_job;

COMMIT;

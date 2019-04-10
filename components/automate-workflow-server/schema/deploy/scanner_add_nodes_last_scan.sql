-- Deploy delivery:scanner_add_nodes_last_scan to pg

BEGIN;

ALTER TABLE nodes
  ADD COLUMN last_job text NOT NULL DEFAULT '';

COMMIT;

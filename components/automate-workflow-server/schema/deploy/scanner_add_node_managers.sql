-- Deploy delivery:scanner_add_node_managers to pg

BEGIN;

CREATE TABLE IF NOT EXISTS node_managers (
  id             TEXT NOT NULL,
  name           TEXT NOT NULL DEFAULT '',
  type           TEXT NOT NULL DEFAULT '',
  credentials    TEXT NOT NULL DEFAULT '',
  status_type    TEXT NOT NULL DEFAULT '',
  status_message TEXT NOT NULL DEFAULT '',
  PRIMARY KEY (id)
);

ALTER TABLE jobs
  ADD COLUMN node_selectors JSON NOT NULL DEFAULT '[]';

COMMIT;

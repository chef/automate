CREATE TABLE IF NOT EXISTS node_managers (
  id             TEXT NOT NULL,
  name           TEXT NOT NULL DEFAULT '',
  type           TEXT NOT NULL DEFAULT '',
  credentials    TEXT NOT NULL DEFAULT '',
  status_type    TEXT NOT NULL DEFAULT '',
  status_message TEXT NOT NULL DEFAULT '',
  PRIMARY KEY (id)
);

ALTER TABLE IF EXISTS jobs
  ADD COLUMN IF NOT EXISTS node_selectors JSON NOT NULL DEFAULT '[]';

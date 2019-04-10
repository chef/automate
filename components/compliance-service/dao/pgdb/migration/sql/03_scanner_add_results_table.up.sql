CREATE TABLE IF NOT EXISTS results (
  job_id     text NOT NULL references jobs(id) ON DELETE CASCADE,
  node_id    text NOT NULL,
  report_id  text NOT NULL DEFAULT '',
  status     text NOT NULL,
  result     text NOT NULL DEFAULT '',
  start_time timestamp DEFAULT '0001-01-01T00:00:00Z00:00',
  end_time   timestamp DEFAULT '0001-01-01T00:00:00Z00:00'
);
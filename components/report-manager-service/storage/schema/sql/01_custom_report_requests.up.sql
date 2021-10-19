CREATE TABLE IF NOT EXISTS custom_report_requests (
  id            text NOT NULL,
  requestor     text NOT NULL,
  status        text NOT NULL,
  message       text,
  created_at    timestamp DEFAULT '0001-01-01T00:00:00Z00:00',
  updated_at    timestamp DEFAULT '0001-01-01T00:00:00Z00:00',
  primary key(id)
);
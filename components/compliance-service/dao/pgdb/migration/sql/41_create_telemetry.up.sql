-- create table telemetry to store the last complaiance telemtry reported date
CREATE TABLE IF NOT EXISTS telemetry (
  id                         text PRIMARY KEY,
  last_telemetry_reported_at TIMESTAMPTZ NOT NULL,
  created_at                 TIMESTAMPTZ NOT NULL,
  updated_at                 TIMESTAMPTZ NOT NULL,
  UNIQUE(last_telemetry_reported_at)
);

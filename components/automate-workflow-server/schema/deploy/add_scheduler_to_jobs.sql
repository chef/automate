-- Deploy delivery:add_scheduler_to_jobs to pg

BEGIN;

ALTER TABLE jobs
  ADD COLUMN scheduled_time timestamp DEFAULT '0001-01-01T00:00:00Z00:00',
  ADD COLUMN recurrence TEXT NOT NULL DEFAULT '',
  ADD COLUMN parent_id TEXT NOT NULL DEFAULT '',
  ADD COLUMN job_count int NOT NULL DEFAULT 0;

COMMIT;

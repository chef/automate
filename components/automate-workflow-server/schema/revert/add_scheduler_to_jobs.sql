-- Revert delivery:add_scheduler_to_jobs from pg

BEGIN;

ALTER TABLE jobs
  DROP COLUMN IF EXISTS scheduled_time,
  DROP COLUMN IF EXISTS recurrence,
  DROP COLUMN IF EXISTS parent_id,
  DROP COLUMN IF EXISTS job_count;

COMMIT;
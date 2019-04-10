-- Verify delivery:add_scheduler_to_jobs on pg

BEGIN;

SELECT
  scheduled_time,
  recurrence,
  parent_id,
  job_count
FROM jobs WHERE FALSE;

ROLLBACK;

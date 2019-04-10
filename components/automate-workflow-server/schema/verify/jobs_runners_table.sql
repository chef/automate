-- Verify delivery:jobs_runners_table on pg

BEGIN;

SELECT id,
       name,
       private_key
  FROM jobs_runners
 WHERE FALSE;

ROLLBACK;

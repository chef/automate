-- Verify os info added to delivery:jobs_runners_table

BEGIN;

SELECT os FROM jobs_runners WHERE FALSE;
SELECT platform_family FROM jobs_runners WHERE FALSE;
SELECT platform FROM jobs_runners WHERE FALSE;
SELECT platform_version FROM jobs_runners WHERE FALSE;

ROLLBACK;

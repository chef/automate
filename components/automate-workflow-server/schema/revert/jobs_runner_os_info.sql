-- Revert os info on delivery:jobs_runners_table

BEGIN;

ALTER TABLE jobs_runners DROP COLUMN os;
ALTER TABLE jobs_runners DROP COLUMN platform_family;
ALTER TABLE jobs_runners DROP COLUMN platform;
ALTER TABLE jobs_runners DROP COLUMN platform_version;

COMMIT;

-- Add os info to delivery:jobs_runners_table

BEGIN;

ALTER TABLE jobs_runners ADD COLUMN os TEXT;
ALTER TABLE jobs_runners ADD COLUMN platform_family TEXT;
ALTER TABLE jobs_runners ADD COLUMN platform TEXT;
ALTER TABLE jobs_runners ADD COLUMN platform_version TEXT;

UPDATE jobs_runners SET os = 'not_migrated';
UPDATE jobs_runners SET platform_family = 'not_migrated';
UPDATE jobs_runners SET platform = 'not_migrated';
UPDATE jobs_runners SET platform_version = 'not_migrated';

ALTER TABLE jobs_runners ALTER COLUMN os SET NOT NULL;
ALTER TABLE jobs_runners ALTER COLUMN platform_family SET NOT NULL;
ALTER TABLE jobs_runners ALTER COLUMN platform SET NOT NULL;
ALTER TABLE jobs_runners ALTER COLUMN platform_version SET NOT NULL;

COMMIT;

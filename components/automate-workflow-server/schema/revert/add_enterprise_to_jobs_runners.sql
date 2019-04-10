-- Revert delivery:add_enterprise_to_jobs_runners from pg

BEGIN;

  ALTER TABLE jobs_runners DROP CONSTRAINT name_enterprise_id_unique;
  ALTER TABLE jobs_runners DROP CONSTRAINT enterprise_id_fk;
  ALTER TABLE jobs_runners ADD CONSTRAINT jobs_runners_name_key UNIQUE (name);
  ALTER TABLE jobs_runners DROP COLUMN enterprise_id;

COMMIT;
